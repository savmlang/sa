use phf::{Map, phf_map};
use std::{
  borrow::Cow,
  collections::{HashMap, HashSet},
};

use crate::{
  GLOB_MACROS, GLOB_VALUES,
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
        #[allow(unused)]
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
        #[allow(unused)]
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
  // Register mapping
  "r1" => OutValue::u8(0),
  "r2" => OutValue::u8(1),
  "r3" => OutValue::u8(2),
  "r4" => OutValue::u8(3),
  "r5" => OutValue::u8(4),
  "r6" => OutValue::u8(5),
  "r7" => OutValue::u8(6),
  "r8" => OutValue::u8(7),
  "scratchpad" => OutValue::u8(8),
  "largepad" => OutValue::u8(9),
  "ptr" => OutValue::u8(10),

  // Count
  "COUNT_ABSOLUTE" => OutValue::u1(0),
  "COUNT_FROM_R1" => OutValue::u1(1),

  // Widths
  "w64" => OutValue::u2(0),
  "w32" => OutValue::u2(1),
  "w16" => OutValue::u2(2),
  "w8" => OutValue::u2(3),

  // JZ,JNZ Flags
  "OP_JZ" => OutValue::u1(0),
  "OP_JNZ" => OutValue::u1(1),

  // Ops
  "IOP_EQ" => OutValue::u5(0),
  "IOP_NEQ" => OutValue::u5(1),
  "IOP_S_LT" => OutValue::u5(2),
  "IOP_U_LT" => OutValue::u5(3),
  "IOP_S_LTEQ" => OutValue::u5(4),
  "IOP_U_LTEQ" => OutValue::u5(5),

  "IOP_S_GT" => OutValue::u5(6),
  "IOP_U_GT" => OutValue::u5(7),
  "IOP_S_GTEQ" => OutValue::u5(8),
  "IOP_U_GTEQ" => OutValue::u5(9),

  "FOP_ORD" => OutValue::u5(10),
  "FOP_UORD" => OutValue::u5(11),
  "FOP_EQ" => OutValue::u5(12),
  "FOP_NEQ" => OutValue::u5(13),
  "FOP_ORD_NEQ" => OutValue::u5(14),
  "FOP_UNORD_NEQ" => OutValue::u5(15),

  "FOP_ORD_LT" => OutValue::u5(16),
  "FOP_ORD_LTEQ" => OutValue::u5(17),
  "FOP_ORD_GT" => OutValue::u5(18),
  "FOP_ORD_GTEQ" => OutValue::u5(19),

  "FOP_UORD_LT" => OutValue::u5(20),
  "FOP_UORD_LTEQ" => OutValue::u5(21),
  "FOP_UORD_GT" => OutValue::u5(22),
  "FOP_UORD_GTEQ" => OutValue::u5(23),

  // SUBOPS

  "VFOP_CEIL" => OutValue::u3(0),
  "VFOP_FLOOR" => OutValue::u3(1),
  "VFOP_TRUNC" => OutValue::u3(2),
  "VFOP_NEAREST" => OutValue::u3(3),
  "VFOP_SQRT" => OutValue::u3(4),

  // BITOP
  "VBIT_AND" => OutValue::u4(0),
  "VBIT_OR" => OutValue::u4(1),
  "VBIT_XOR" => OutValue::u4(2),
  "VBIT_NOT" => OutValue::u4(3),
  "VBIT_OR_NOT" => OutValue::u4(4),
  "VBIT_AND_NOT" => OutValue::u4(5),
  "VBIT_XOR_NOT" => OutValue::u4(6),
  "VBIT_BITREV" => OutValue::u4(7),
  "VBIT_BSWAP" => OutValue::u4(8),

  // SH
  "SH_OP_SHL" => OutValue::u1(0),
  "SH_OP_SHR" => OutValue::u1(1),

  // VMINIMAX
  "OP_VMIN" => OutValue::u1(0),
  "OP_VMAX" => OutValue::u1(1),

  // VCNT
  "VCNT_OP_POPCNT" => OutValue::u4(0),
  "VCNT_OP_CLZ" => OutValue::u4(1),
  "VCNT_OP_CLS" => OutValue::u4(2),
  "VCNT_OP_CTZ" => OutValue::u4(3),

  // VTASK
  "VTASK_ASYNC_DETACH" => OutValue::u4(0),
  "VTASK_ASYNC_JOIN" => OutValue::u4(1),
  "VTASK_ASYNC_ISCOMPLETE" => OutValue::u4(2),
  "VTASK_SYNC_DETACH" => OutValue::u4(3),
  "VTASK_SYNC_JOIN" => OutValue::u4(4),
  "VTASK_SYNC_ISCOMPLETE" => OutValue::u4(5),
  "VTASK_THREAD_UNPARK" => OutValue::u4(6),
  "VTASK_SYNC_THREAD_UNPARK" => OutValue::u4(7),
  "VTASK_SYNC_THREAD_DETACH" => OutValue::u4(8),
  "VTASK_SYNC_YIELD" => OutValue::u4(9),
  "VTASK_ASYNC_YIELD" => OutValue::u4(10),
  "VTASK_WAITMS" => OutValue::u4(11),

  // ORD
  "SEQCST" => OutValue::u3(0),
  "RELAXED" => OutValue::u3(1),
  "ACQUIRE" => OutValue::u3(2),
  "RELEASE" => OutValue::u3(3),
  "ACQ_REL" => OutValue::u3(4),

  // ATOMIC OP
  "OP_CAS" => OutValue::u2(0),
  "OP_LOAD" => OutValue::u2(1),
  "OP_RMW" => OutValue::u2(2),
  "OP_STORE" => OutValue::u2(3),

  // Types
  "u64" => OutValue::u4(0),
  "u32" => OutValue::u4(1),
  "u16" => OutValue::u4(2),
  "u8" => OutValue::u4(3),
  "i64" => OutValue::u4(4),
  "i32" => OutValue::u4(5),
  "i16" => OutValue::u4(6),
  "i8" => OutValue::u4(7),
  "f64" => OutValue::u4(8),
  "f32" => OutValue::u4(9),

};

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum Condition {
  Mustuse,
}

pub struct State<'a> {
  pub resolved: HashMap<&'a str, OutValue>,
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

      // User wants to import a macro from
      if id == "*" {
        IMPORTS.entries().for_each(|(k, v)| {
          if let Some(_) = hmap.insert(*k, *v) {
            panic!("While loading the whole prelude, found duplicate : {k}");
          }
        });
      } else if id.starts_with("#") {
        let Some(mc) = GLOB_MACROS
          .get()
          .expect("`macros` cannot globally import macros!")
          .get(id)
        else {
          panic!("Unable to resolve global macro : {id}");
        };

        state.macros.insert(id, Cow::Borrowed(mc));
      } else if let Some(_) = hmap.insert(
        id,
        GLOB_VALUES
          .get()
          .and_then(|x| x.get(&id))
          .map(|x| *x)
          .or_else(|| IMPORTS.get(id).map(|x| *x))
          .or_else(|| {
            id.strip_suffix("::")
              .and_then(|x| IMPORTS.get(x))
              .map(|x| *x)
          })
          .expect("Unable to find identifier from either GLOBAL_VALUES or DEFAULT_IMPORTS"),
      ) {
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
