use std::rc::Rc;

use sair::{
  SingleThreadedStringStore,
  mir::{
    Module,
    value::{
      BaseType, ValueType,
      consts::{D64, F32, I32, I64},
      sig::Signature,
    },
  },
};

fn main() {
  let store = SingleThreadedStringStore::new();
  let mut module = Module::new(&store, "MyModule");

  module.insert_type(ValueType::Vector {
    base: BaseType::UInt64,
    count: 32,
  });

  module.insert_type(ValueType::PrimaryUnion {
    composition: {
      let mut o = [BaseType::Double64; 36];

      o[0] = BaseType::Double64;
      o[1] = BaseType::Float32;

      o
    },
    count: 2,
    align: None,
  });

  let a = module.insert_type(ValueType::PrimaryComposite {
    composition: {
      let mut o = [BaseType::Double64; 36];

      o[0] = BaseType::Double64;
      o[1] = BaseType::Float32;
      o[2] = BaseType::UInt8;

      o
    },
    count: 3,
    align: None,
  });

  let b = module.insert_type(ValueType::Union {
    composition: Rc::new([a, I32, F32]),
    align: None,
  });

  module.insert_type(ValueType::Composite {
    composition: Rc::new([I64, D64, a, b]),
    align: None,
  });

  let sig = Signature::new(&mut module, &[I64], None).unwrap();
  let sig = module.signature(sig);

  module.import("hello2", sig);
  module.export_fn("hello");

  {
    let mut myfn = module.function("hello", sig);

    let mut builder = myfn.builder(&module);
    builder.block(&[I64, D64, a]);

    _ = module.add_function(myfn);
  }

  println!("{module:?}");
}
