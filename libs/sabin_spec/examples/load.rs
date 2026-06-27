use sabin_spec::file::{IRamFile, RamFile};

fn main() {
  let file = RamFile::open("./examples/hello.world").unwrap();

  println!("{:?}", file.as_slice());
}
