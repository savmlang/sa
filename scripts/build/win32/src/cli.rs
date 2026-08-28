use std::{
  env::args,
  io::{self, BufRead, Write},
  process::exit,
};

use windows::Win32::System::Com::{CoInitialize, CoUninitialize};

use crate::inst::{Config, SDKConfig, ToolConfig, WinConfig, install_info, uninstall};

mod inst;

fn main() {
  let guard = unsafe { ComGuard(CoInitialize(None).is_ok()) };

  let args = args().collect::<Vec<_>>();

  if let Some(_) = args.iter().find(|x| x as &str == "uninstall") {
    uninstall::<_, false>(|| {});
    println!("Successfully uninstalled! A reboot is required to completely clean up stray files.");
    return;
  }

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

  if let Some(_) = args.iter().find(|x| x as &str == "help") {
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

  let prompt = args.iter().any(|x| x as &str == "--prompt");
  let mut argv = args;
  if prompt {
    let mut stdout = io::stdout().lock();
    _ = stdout.write_all(b"\nEnter arguments : ");
    _ = stdout.flush();

    let mut buf = String::default();
    io::stdin().lock().read_line(&mut buf).unwrap();

    argv = buf.split_whitespace().map(ToOwned::to_owned).collect();
  }

  for arg in argv {
    if arg.contains("=") && arg.starts_with("--") {
      let (key, val) = arg.strip_prefix("--").unwrap().split_once("=").unwrap();

      let val = match val.trim().to_ascii_lowercase().as_str() {
        "true" | "1" => true,
        "false" | "0" => false,
        _ => {
          eprintln!("Invalid boolean for --{key}: expected true/false, got '{val}'");
          exit(-1);
        }
      };

      *match key {
        "headers" => &mut config.sdk.headers,
        "linklibs" => &mut config.sdk.linklibs,
        "staticarchives" => &mut config.sdk.staticarchives,

        "satest" => &mut config.tools.satest,
        "saapprt" => &mut config.tools.saapprt,

        "path" => &mut config.w32.path,
        "start" => &mut config.w32.start,

        "prompt" => continue,

        e => {
          eprintln!("Unknown key: --{e}");
          exit(-1);
        }
      } = val;
    }
  }

  println!("\nSelected Configuration:\n{config:#?}");

  install_info::<_, _, false>(
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

  drop(guard);
}

struct ComGuard(bool);
impl Drop for ComGuard {
  fn drop(&mut self) {
    if self.0 {
      unsafe { CoUninitialize() };
    }
  }
}
