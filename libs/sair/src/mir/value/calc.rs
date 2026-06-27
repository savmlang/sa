use crate::{
  StringStore,
  mir::{
    Module,
    value::{Alignment, BaseType, ValueType},
  },
};

impl ValueType {
  pub fn size<T: StringStore>(&self, module: &Module<T>) -> usize {
    match self {
      Self::Base { base, .. } => width(base),
      Self::Vector { base, count } => width(base) * (*count as usize),

      Self::PrimaryUnion {
        composition, count, ..
      } => composition
        .get(0..(*count as usize))
        .unwrap_or(&[])
        .iter()
        .map(width)
        .max()
        .unwrap_or(8)
        .next_multiple_of(self.align(module)),

      Self::Union { composition, .. } => composition
        .iter()
        .copied()
        .filter_map(|x| module.type_data(x))
        .map(|x| x.size(module))
        .max()
        .unwrap_or(8)
        .next_multiple_of(self.align(module)),

      Self::PrimaryComposite {
        composition, count, ..
      } => {
        let unpadded_size = composition
          .get(0..(*count as usize))
          .unwrap_or(&[])
          .iter()
          .fold(0usize, |current_size, field_ty| {
            let field_align = width(field_ty);
            current_size.next_multiple_of(field_align) + width(field_ty)
          });

        unpadded_size.next_multiple_of(self.align(module))
      }

      Self::Composite { composition, .. } => {
        let unpadded_size = composition
          .as_ref()
          .iter()
          .filter_map(|&vtr| module.type_data(vtr))
          .fold(0usize, |current_size, field_ty| {
            let field_align = field_ty.align(module);
            current_size.next_multiple_of(field_align) + field_ty.size(module)
          });

        unpadded_size.next_multiple_of(self.align(module))
      }
    }
  }

  pub fn align<T: StringStore>(&self, module: &Module<T>) -> usize {
    match self {
      Self::Base { base, .. } => width(base),

      // In our VM - the alignment is not necessary for vectors
      Self::Vector { base, .. } => width(base),

      // For union - alignment == largest one
      Self::PrimaryUnion {
        composition,
        count,
        align,
      } => align.map(Alignment::align).unwrap_or_else(|| {
        composition
          .get(0..(*count as usize))
          .unwrap_or(&[])
          .iter()
          .map(width)
          .max()
          .unwrap_or(8)
      }),

      // For primary structs: alignment == largest one
      Self::PrimaryComposite {
        composition,
        count,
        align,
      } => align.map(Alignment::align).unwrap_or_else(|| {
        composition
          .get(0..(*count as usize))
          .unwrap_or(&[])
          .iter()
          .map(width)
          .max()
          .unwrap_or(8)
      }),

      Self::Composite { composition, align } => align.map(Alignment::align).unwrap_or_else(|| {
        composition
          .iter()
          .copied()
          .filter_map(|x| module.type_data(x))
          .map(|x| x.align(module))
          .max()
          .unwrap_or(8)
      }),

      Self::Union { composition, align } => align.map(Alignment::align).unwrap_or_else(|| {
        composition
          .iter()
          .copied()
          .filter_map(|x| module.type_data(x))
          .map(|x| x.align(module))
          .max()
          .unwrap_or(8)
      }),
    }
  }
}

fn width(base: &BaseType) -> usize {
  match base {
    BaseType::UInt64 | BaseType::Int64 | BaseType::Double64 => 8,
    BaseType::Float32 | BaseType::Int32 | BaseType::UInt32 => 4,
    BaseType::Int16 | BaseType::UInt16 => 2,
    BaseType::Int8 | BaseType::UInt8 => 1,
  }
}
