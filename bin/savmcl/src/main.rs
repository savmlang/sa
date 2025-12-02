use std::fs::{self, create_dir_all, read_dir, remove_dir_all};

use rayon::iter::{IntoParallelIterator, ParallelIterator};
use sart::ctr::parse_instrution;

use crate::compile::AssemblyParser;

mod compile;

fn main() {
  _ = remove_dir_all("./out");
  create_dir_all("./out").unwrap();

  let entries = read_dir("./bin")
    .unwrap()
    .into_iter()
    .map(|x| x.unwrap())
    .collect::<Vec<_>>();

  fs::create_dir_all("./out/1/").unwrap();
  fs::write("./out/1/_std", "stdlib").unwrap();

  entries.into_par_iter().for_each(|x| {
    let mut parser = AssemblyParser::new();

    let id = x
      .file_name()
      .into_string()
      .unwrap()
      .replace(".o", "")
      .parse::<u32>()
      .unwrap();

    _ = fs::create_dir_all(format!("./out/{id}"));

    parser.parse(
      fs::read_to_string(format!("./bin/{}", x.file_name().into_string().unwrap()))
        .unwrap()
        .as_str(),
    );

    parser.get_functions().into_iter().for_each(|(key, value)| {
      let fid = key.parse::<u32>().unwrap();

      let mut code: Vec<u8> = vec![];

      value.instructions.clone().into_iter().for_each(|inst| {
        let splits = inst.split(" ").collect::<Vec<_>>();

        let [inst, parts @ ..] = &splits[..] else {
          code.push(parse_instrution(&splits[0]).unwrap());
          return;
        };

        code.push(sart::ctr::parse_instrution(*inst).unwrap());

        for part in parts {
          let (parta, partb) = part.split_once("x").unwrap();

          match parta {
            "8" => {
              code.extend(partb.parse::<u8>().unwrap().to_be_bytes());
            }
            "64" => {
              code.extend(partb.parse::<u64>().unwrap().to_be_bytes());
            }
            _ => unimplemented!(),
          }
        }
      });

      fs::write(format!("./out/{id}/{fid}"), code).unwrap();
    });
  });
}
