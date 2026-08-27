use std::{env::args, process::exit};

use crate::inst::{Config, SDKConfig, ToolConfig, WinConfig, install_info, uninstall};

mod inst;

fn main() {
  let args = args().collect::<Vec<_>>();

  if let Some(_) = args.iter().find(|x| x.contains("help")) {
    println!(
      "<binary> help/uninstall/repair/<empty> [args]
Args:
  --headers=true/false
  --linklibs=true/false
  --staticarchives=true/false
  --satest=true/false
  --saapprt=true/false
  --path=true/false
  --start=true/false"
    );
    return;
  }
  if let Some(_) = args.iter().find(|x| x as &str == "uninstall") {
    uninstall::<_, false>(|| {});
    println!("Successfully uninstalled! A reboot is required to completely clean up stray files.");
    return;
  }

  let mut repair = false;
  if let Some(_) = args.iter().find(|x| x as &str == "repair") {
    println!("Use --<key>=<true/false> to specify configuration");
    repair = true;
  }

  let mut config = Config {
    sdk: SDKConfig {
      headers: true,
      linklibs: true,
      staticarchives: true,
    },
    tools: ToolConfig {
      satest: true,
      saapprt: true,
    },
    w32: WinConfig {
      path: true,
      start: true,
    },
  };

  for arg in args {
    if arg.contains("=") && arg.starts_with("--") {
      let (key, val) = arg.strip_prefix("--").unwrap().split_once("=").unwrap();

      let val = if val == "false" { false } else { true };

      *match key {
        "headers" => &mut config.sdk.headers,
        "linklibs" => &mut config.sdk.linklibs,
        "staticarchives" => &mut config.sdk.staticarchives,

        "satest" => &mut config.tools.satest,
        "saapprt" => &mut config.tools.saapprt,

        "path" => &mut config.w32.path,
        "start" => &mut config.w32.start,

        e => {
          eprintln!("Unknown key: --{e}");
          exit(-1);
        }
      } = val;
    }
  }

  install_info::<_, _, false, true>(
    |x, y| {
      println!("({:06.2}%) {x}", y * 100.0);
    },
    || {
      if repair {
        println!("Successfully Repaired!");
      } else {
        println!("Successfully Installed!");
      }
    },
    config,
    repair,
  );
}
