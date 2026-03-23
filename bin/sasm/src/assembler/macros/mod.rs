use std::collections::{HashMap, HashSet};

use crate::assembler::OutValue;

#[derive(Debug, Clone, Copy)]
pub enum AssertOp {
  Or,
  And,
}

#[derive(Debug, Clone)]
pub struct MacroJIT<'a> {
  pub mustuse: HashSet<&'a str>,
  pub total_args: usize,
  pub asserts: Box<[(AssertOp, Box<[(usize, u8)]>)]>,
  pub reloctable: HashMap<usize, Box<[usize]>>,

  compiled: Vec<u8>,
}

impl<'a> MacroJIT<'a> {
  pub fn from_builder(builder: MicroJITBuilder<'a>) -> (&'a str, Self) {
    let name = builder.name;

    (
      name,
      Self {
        mustuse: builder.mustuse,
        asserts: builder.asserts.into_boxed_slice(),
        reloctable: builder
          .reloctable
          .into_iter()
          .map(|(k, v)| (k, v.into_boxed_slice()))
          .collect::<_>(),
        total_args: builder.args.len(),
        compiled: builder.compiled,
      },
    )
  }

  pub fn write<'b>(
    &self,
    out: &mut Vec<u8>,
    args: &[OutValue],
    mut macro_mode: Option<&mut MicroJITBuilder<'b>>,
  ) {
    if self.total_args != args.len() {
      panic!(
        "Args mismatch for macro : Expected {}, found {}",
        self.total_args,
        args.len()
      );
    }

    self.asserts.iter().for_each(|(d, op)| {
      let mut ite = op.iter();

      let cond = |(arg, size): &(usize, u8)| {
        // Found a macro argument, pass any width checks
        if *size == 0 {
          return true;
        }

        args[*arg].width == *size
      };

      if !match *d {
        // OR
        AssertOp::Or => ite.any(cond),
        // AND
        AssertOp::And => ite.all(cond),
      } {
        panic!("Assertion failed : {op:?}");
      }
    });

    for (idx, data) in self.compiled.iter().enumerate() {
      out.push(*data);

      if let Some(table) = self.reloctable.get(&idx) {
        for item in table {
          let val = { *args.get(*item).unwrap() };

          match (val.width, macro_mode.as_mut()) {
            (0, Some(mc)) => {
              (*mc)
                .reloctable
                .entry(out.len().checked_sub(1).expect(
                  "First operation cannot call a macro, consider adding anything meaningful beforehand.",
                ))
                .or_default()
                .push(val.data as _);
            }
            (8, _) => out.extend_from_slice(&val.as_u8().to_le_bytes()),
            (16, _) => out.extend_from_slice(&val.as_u16().to_le_bytes()),
            (32, _) => out.extend_from_slice(&val.as_u32().to_le_bytes()),
            (64, _) => out.extend_from_slice(&val.as_u64().to_le_bytes()),
            other => panic!(
              "Illegal bitwidth : u{}. Ensure that operands are Quantizable (u8, u16, u32, u64)",
              other.0
            ),
          }
        }
      }
    }
  }
}

#[derive(Debug)]
pub struct MicroJITBuilder<'a> {
  pub name: &'a str,

  pub resolved: HashMap<&'a str, OutValue>,
  pub mustuse: HashSet<&'a str>,

  pub args: Box<[&'a str]>,
  pub asserts: Vec<(AssertOp, Box<[(usize, u8)]>)>,

  // Relocation Table
  // {index} -> {argid}
  //
  // This is how it is meant to be used!
  //
  // Write the data of index {index}, then write all the values of the arguments at these indices!
  pub reloctable: HashMap<usize, Vec<usize>>,

  pub compiled: Vec<u8>,
}
