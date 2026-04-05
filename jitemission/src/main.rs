use savm::acaot::{native::compiler_infra, pickle::PickleWorker};

fn main() {
  let f = std::fs::File::open("./2").unwrap();

  let mut worker = PickleWorker {
    bytecode: f,
    out: vec![],
    jump: Default::default(),
  };

  worker.pass1();

  let mut cranelift = compiler_infra()[0].get_abs8();
  cranelift.compile(&worker.out, &worker.jump);
}
