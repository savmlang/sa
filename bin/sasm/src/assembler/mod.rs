#![allow(unused)]

use phf::{Map, phf_map};
use std::{
  borrow::Cow,
  collections::{HashMap, HashSet},
};

use crate::{
  GLOB_MACROS,
  assembler::{
    macros::{AssertOp, MacroJIT, MicroJITBuilder},
    number::parse_expr,
  },
};

use sart::ctr::parse_instrution;

pub mod macros;
mod number;

#[derive(Debug, Clone, Copy)]
pub struct OutValue {
  pub data: u64,
  pub width: u8,
}

macro_rules! widths {
  (
    $($width:ident $as:ident => { $t:ty }$( size=$size:expr)?),*
  ) => {
    $(
      pub const fn $width(data: $t) -> Self {
        #[allow(unused_mut)]
        let size = std::mem::size_of::<$t>()*8;

        $(
          let size = $size;
        )?

        if size < 64 {
          assert!((data as u64) < (2 as u64).saturating_pow(size as _));
        }

        Self {
          data: data as u64,
          width: size as _,
        }
      }

      pub const fn $as(self) -> $t {
        #[allow(unused_mut)]
        let size = std::mem::size_of::<$t>()*8;

        $(
          let size = $size;
        )?

        assert!(self.width >= (size as _));

        let output = self.data & Self::mask(self.width as _);
        let out_wd = self.data & Self::mask(size as _);

        assert!(output as u64 == out_wd as u64);

        out_wd as _
      }
    )*
  };
}

impl OutValue {
  pub const fn mask(width: u8) -> u64 {
    assert!(width <= 64);

    if width == 64 {
      return !0;
    }

    !(!0 << width)
  }

  pub const fn as_width(self, size: u8) -> u64 {
    assert!(self.width >= (size as _));

    let output = self.data & Self::mask(self.width as _);
    let out_wd = self.data & Self::mask(size as _);

    assert!(output as u64 == out_wd as u64);

    out_wd as _
  }

  pub const fn into_width(self, size: u8) -> Self {
    Self {
      data: self.as_width(size),
      width: size,
    }
  }

  widths! {
    u1 as_u1 => { u8 } size=1,
    u2 as_u2 => { u8 } size=2,
    u3 as_u3 => { u8 } size=3,
    u4 as_u4 => { u8 } size=4,
    u5 as_u5 => { u8 } size=5,
    u6 as_u6 => { u8 } size=6,
    u7 as_u7 => { u8 } size=7,
    u8 as_u8 => { u8 },
    u16 as_u16 => { u16 },
    u32 as_u32 => { u32 },
    u64 as_u64 => { u64 }
  }
}

static IMPORTS: Map<&'static str, OutValue> = phf_map! {
  "r1" => OutValue::u8(0),
  "r2" => OutValue::u8(1),
  "r3" => OutValue::u8(2),
  "r4" => OutValue::u8(3),
  "r5" => OutValue::u8(4),
  "r6" => OutValue::u8(5),
  "r7" => OutValue::u8(6),
  "r8" => OutValue::u8(7),
};

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum Condition {
  Mustuse,
}

pub struct State<'a> {
  resolved: HashMap<&'a str, OutValue>,
  suppress: HashSet<Condition>,
  macro_used: HashSet<&'a str>,
  must_use: HashSet<&'a str>,
  pub out: Vec<u8>,

  pub macros: HashMap<&'a str, Cow<'a, MacroJIT<'a>>>,
  curr_macro: Option<MicroJITBuilder<'a>>,
}

pub fn assemble<'a>(data: &'a str) -> State<'a> {
  let mut state = State {
    resolved: HashMap::new(),
    suppress: HashSet::new(),
    must_use: HashSet::new(),
    macro_used: HashSet::new(),
    out: Vec::with_capacity(data.len()),

    macros: HashMap::new(),
    curr_macro: None,
  };

  let copies = data
    .lines()
    .map(|x| x.trim())
    .filter(|x| !x.is_empty() && !x.starts_with(";"));

  // Parse main code
  let macromode = copies.fold(false, |macromode, statement| {

    println!("{macromode}: {statement:?}");

    let mut final_macromode = macromode;

    match statement.chars().next().unwrap() {
      // Definition
      '#' => parse_pwr(statement, &mut state, &mut final_macromode),
      _ => {
        let Some((instr, ops)) = statement.split_once(" ") else {
          let instr = parse_instrution(statement.trim()).expect("Unknown intruction found");
          let outvect = if macromode {
            &mut state.curr_macro.as_mut().unwrap().compiled
          } else {
            &mut state.out
          };
          outvect.push(instr);
          return final_macromode;
        };

        // Push instruction
        let instr = parse_instrution(instr).expect("Unknown intruction found");
        let outvect = if macromode {
          &mut state.curr_macro.as_mut().unwrap().compiled
        } else {
          &mut state.out
        };
        outvect.push(instr);

        ops.trim().split(",").map(|x| x.trim()).for_each(|x| {
          let val = parse_expr(&mut state, x, macromode, false);
          let outvect = if macromode {
            &mut state.curr_macro.as_mut().unwrap().compiled
          } else {
            &mut state.out
          };

          match val.width {
            0 => if !macromode {
              panic!(
                "Illegal bitwidth : u0. Ensure that operands are Quantizable (u8, u16, u32, u64)"
              );
            }
            8 => outvect.extend_from_slice(&val.as_u8().to_le_bytes()),
            16 => outvect.extend_from_slice(&val.as_u16().to_le_bytes()),
            32 => outvect.extend_from_slice(&val.as_u32().to_le_bytes()),
            64 => outvect.extend_from_slice(&val.as_u64().to_le_bytes()),
            other => panic!(
              "Illegal bitwidth : u{other}. Ensure that operands are Quantizable (u8, u16, u32, u64)"
            ),
          }
        });
      }
    }

    final_macromode
  });

  if macromode {
    panic!("Found an incomplete macro!");
  }

  if !state.suppress.contains(&Condition::Mustuse) {
    let o = state
      .must_use
      .iter()
      .map(|x| *x)
      .fold(vec![], |mut vect, mc| {
        if !state.macro_used.contains(mc) {
          vect.push(mc);
        }

        vect
      });

    if !o.is_empty() {
      panic!("#mustuse violation : Macros `{:?}` have not been used!", o);
    }
  }

  state
}

fn parse_pwr<'a>(statement: &'a str, state: &mut State<'a>, macromode: &mut bool) {
  let (inst, payload) = statement.split_once(" ").map_or_else(
    || {
      if statement == "#end" {
        return ("#end", "");
      }

      panic!("Error : Unknown Directive format!");
    },
    |x| x,
  );

  match inst {
    "#define" => {
      let (varname, data) = payload.trim().split_once(" ").expect("Unknown format!");

      let out = parse_expr(state, data, *macromode, false);

      let hmap = if *macromode {
        &mut state.curr_macro.as_mut().unwrap().resolved
      } else {
        &mut state.resolved
      };

      if let Some(_) = hmap.insert(varname, out) {
        panic!("Duplicate identifier : {varname}");
      }
    }

    "#import" => payload.split(",").map(|x| x.trim()).for_each(|id| {
      let hmap = if *macromode {
        &mut state.curr_macro.as_mut().unwrap().resolved
      } else {
        &mut state.resolved
      };

      // User wants to import a macro from globals
      if id.starts_with("#") {
        let Some(mc) = GLOB_MACROS
          .get()
          .expect("`macros` cannot globally import macros!")
          .get(id)
        else {
          panic!("Unable to resolve global macro : {id}");
        };

        state.macros.insert(id, Cow::Borrowed(mc));
      } else if let Some(_) = hmap.insert(id, *IMPORTS.get(id).expect("Unable to resolve")) {
        panic!("Duplicate identifier : {id}");
      };
    }),

    "#suppress" => {
      if *macromode {
        panic!("#suppress is not allowed in macros");
      }

      payload
        .trim()
        .split(",")
        .map(|x| x.trim())
        .for_each(|x| match x {
          "mustuse" => {
            state.suppress.insert(Condition::Mustuse);
          }
          unknown => panic!("Unknown rule : {unknown}"),
        });
    }

    "#mustuse" => {
      if !*macromode {
        panic!("Error : #mustuse is a macro-only directive");
      }

      payload.trim().split(",").map(|x| x.trim()).for_each(|x| {
        state.curr_macro.as_mut().unwrap().mustuse.insert(x);
      });
    }

    "#assert" => {
      if !*macromode {
        panic!("Error : Assert is a macro-only operation!");
      }

      let parsecond = |pl: &str| {
        let (arg, wid) = pl.trim().split_once(" ").unwrap();
        let wid = wid.parse::<u8>().unwrap();

        let mc = state.curr_macro.as_ref().unwrap();

        let idx = mc
          .args
          .iter()
          .position(|x| *x == arg)
          .expect("Unable to get position of operand");

        (idx, wid)
      };

      let Some((op, conds)) = payload.trim().split_once(",") else {
        let op = AssertOp::Or;

        let pr = parsecond(payload);
        state
          .curr_macro
          .as_mut()
          .unwrap()
          .asserts
          .push((op, Box::new([pr])));
        return;
      };

      let op = match op {
        "or" => AssertOp::Or,
        "and" => AssertOp::And,
        op => panic!("Unknown op {op} : expected `or`, `and`"),
      };

      let i = conds.split(",").map(|x| x.trim()).map(parsecond).collect();

      state.curr_macro.as_mut().unwrap().asserts.push((op, i));
    }

    "#macro" => {
      if *macromode {
        panic!("Error : Macro inside macro is not supported!");
      }

      *macromode = true;

      let (macroname, argv) = payload
        .trim()
        .split_once(" ")
        .expect("Invalid macro syntax : #macro #mymacro @1");

      state.curr_macro = Some(MicroJITBuilder {
        name: macroname,
        resolved: Default::default(),
        mustuse: Default::default(),
        args: argv
          .trim()
          .split(",")
          .map(|x| x.trim())
          .filter(|x| !x.is_empty())
          .collect::<_>(),
        reloctable: Default::default(),
        compiled: Default::default(),
        asserts: Default::default(),
      });
    }

    "#end" => {
      if !*macromode {
        panic!("Error : No macro was running");
      }

      *macromode = false;

      let macrobuilder = state.curr_macro.take().unwrap();

      let (name, mjit) = MacroJIT::from_builder(macrobuilder);
      state.macros.insert(name, Cow::Owned(mjit));
    }

    other => {
      let Some(macrodata) = state.macros.get(other) else {
        panic!("Unknown directive : {other}. Neither any associated macros were found.");
      };

      let macrodata = macrodata.as_ref() as *const MacroJIT<'a>;

      let argv = payload
        .split(",")
        .map(|x| x.trim())
        .filter(|x| !x.is_empty())
        .map(|x| parse_expr(state, x, *macromode, true))
        .collect::<Box<[_]>>();

      let macrodata = unsafe { &*macrodata };

      macrodata.write(
        &mut state.out,
        &argv,
        if *macromode {
          Some(state.curr_macro.as_mut().unwrap())
        } else {
          None
        },
      );
      _ = state.macro_used.insert(other);

      macrodata.mustuse.iter().for_each(|e| {
        state.must_use.insert(*e);
      });
    }
  }
}
