use std::{collections::HashMap, fmt::Debug, marker::PhantomData, num::NonZeroUsize};

use rapidhash::{HashMapExt, RapidHashMap};

use crate::{
  StringRef, StringStore,
  mir::{
    function::Function,
    value::{
      BaseType, ValueType, ValueTypeRef,
      sig::{Signature, SignatureRef},
    },
  },
};

pub mod block;
pub mod function;
pub mod ssa;
pub mod value;

pub struct Module<'a, T: StringStore> {
  pub store: &'a T,

  imports: HashMap<StringRef<'a, T>, SignatureRef, rapidhash::fast::RandomState>,
  exports: Vec<StringRef<'a, T>>,

  typemap: Vec<ValueType>,

  sigs: Vec<Signature>,
  functions: HashMap<StringRef<'a, T>, function::Function<'a, T>, rapidhash::fast::RandomState>,

  name: StringRef<'a, T>,
}

impl<'a, T: StringStore> Module<'a, T> {
  pub fn new(store: &'a T, name: &str) -> Self {
    let mut typemap: Vec<ValueType> = [
      BaseType::Int64,
      BaseType::Int32,
      BaseType::Int16,
      BaseType::Int8,
      // Uints
      BaseType::UInt64,
      BaseType::UInt32,
      BaseType::UInt16,
      BaseType::UInt8,
      // floats
      BaseType::Double64,
      BaseType::Float32,
    ]
    .into_iter()
    .map(|base| ValueType::Base {
      base,
      _uninstantiable: PhantomData,
    })
    .collect();

    typemap.reserve(64);

    Self {
      name: store.matchval(name),
      store,

      typemap,
      sigs: Vec::with_capacity(8),
      imports: RapidHashMap::with_capacity(32),
      exports: Vec::with_capacity(32),

      functions: Default::default(),
    }
  }

  pub fn import(&mut self, symbol: &str, sig: SignatureRef) {
    _ = self.imports.insert(self.store.matchval(symbol), sig);
  }

  pub fn signature(&mut self, sig: Signature) -> SignatureRef {
    let idx = self.sigs.len();

    self.sigs.push(sig);

    SignatureRef(idx)
  }

  /// Creates a new Function structure
  ///
  /// The created function is NOT added to this module's functions list
  /// to do that use [Module::add_function]
  pub fn function(&mut self, name: &str, sig: SignatureRef) -> Function<'a, T> {
    let name = self.store.matchval(name);

    Function::new(self.store, name, sig)
  }

  /// Adds the function or returns None if there is a name collision
  pub fn add_function(&mut self, f: Function<'a, T>) -> Option<()> {
    if self.functions.contains_key(&f.name) {
      return None;
    }

    _ = self.functions.insert(f.name, f);
    Some(())
  }

  /// ## Please Note:
  /// Inserting Duplicate ValueType will **NOT** merge them into the same
  /// [ValueTypeRef] and worse than that - our verifier will **NOT** treat
  /// the two as equal types.
  pub fn insert_type(&mut self, t: ValueType) -> ValueTypeRef {
    self.typemap.push(t);

    unsafe { ValueTypeRef(NonZeroUsize::new_unchecked(self.typemap.len())) }
  }

  pub fn type_data(&self, id: ValueTypeRef) -> Option<&ValueType> {
    self.typemap.get(id.index())
  }

  /// Mark the function symbol to be exported
  ///
  /// This does not check if the function with the symbol currently exists or not
  pub fn export_fn(&mut self, symbol: &str) {
    self.exports.push(self.store.matchval(symbol));
  }

  pub fn name(&self) -> StringRef<'a, T> {
    self.name
  }

  pub fn functions(
    &self,
  ) -> &HashMap<StringRef<'a, T>, function::Function<'a, T>, rapidhash::fast::RandomState> {
    &self.functions
  }

  pub fn imports(&self) -> impl Iterator<Item = (&StringRef<'a, T>, &SignatureRef)> {
    self.imports.iter()
  }

  pub fn exports(&self) -> &[StringRef<'a, T>] {
    &self.exports
  }
}

impl<'a, T: StringStore> Debug for Module<'a, T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(
      f,
      "define module {} {{",
      self.store.resolve(self.name).as_ref()
    )?;

    writeln!(f, "; Types (size, align)")?;
    for (id, valtype) in self.typemap.iter().enumerate() {
      valtype.fmt(id + 1, self, f)?;
    }
    writeln!(f, "")?;

    writeln!(f, "; Signatures")?;
    for (idx, sig) in self.sigs.iter().enumerate() {
      sig.print(idx, self, f)?;
    }
    writeln!(f, "")?;

    writeln!(f, "; Imports & Exports")?;
    for (&import, a) in self.imports.iter() {
      writeln!(
        f,
        "  #import {} (@sig:#{})",
        self.store.resolve(import).as_ref(),
        a.0 + 1
      )?;
    }
    for &export in &self.exports {
      writeln!(f, "  #export {}", self.store.resolve(export).as_ref())?;
    }
    writeln!(f, "")?;

    writeln!(f, "; Functions")?;
    for (_, function) in &self.functions {
      function.print(f, self)?;
    }
    writeln!(f, "")?;

    writeln!(f, "}}")?;

    Ok(())
  }
}
