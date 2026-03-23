use std::collections::{HashMap, HashSet};

use crate::assembler::OutValue;

use super::State;

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

  pub fn write(&self, out: &mut Vec<u8>, args: &[OutValue]) {
    if self.total_args != args.len() {
      panic!(
        "Args mismatch for macro : Expected {}, found {}",
        self.total_args,
        args.len()
      );
    }

    self.asserts.iter().for_each(|(d, op)| {
      let mut ite = op.iter();

      let cond = |(arg, size): &(usize, u8)| args[*arg].width == *size;

      if !match *d {
        // OR
        AssertOp::Or => ite.any(cond),
        // AND
        AssertOp::And => ite.all(cond),
        _ => false,
      } {
        panic!("Assertion failed : {op:?}");
      }
    });

    for (idx, data) in self.compiled.iter().enumerate() {
      out.push(*data);

      if let Some(table) = self.reloctable.get(&idx) {
        for item in table {
          let val = { *args.get(*item).unwrap() };

          match val.width {
            8 => out.extend_from_slice(&val.as_u8().to_le_bytes()),
            16 => out.extend_from_slice(&val.as_u16().to_le_bytes()),
            32 => out.extend_from_slice(&val.as_u32().to_le_bytes()),
            64 => out.extend_from_slice(&val.as_u64().to_le_bytes()),
            other => panic!(
              "Illegal bitwidth : u{other}. Ensure that operands are Quantizable (u8, u16, u32, u64)"
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
