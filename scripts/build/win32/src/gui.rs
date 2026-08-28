#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod inst;

use i_slint_backend_winit::winit::platform::windows::{BackdropType, WindowAttributesExtWindows};
use slint::{Brush, Color, Model, SharedString, VecModel};
use std::{env::args, rc::Rc, sync::OnceLock, thread};
use windows_version::*;

use crate::inst::{Config, SDKConfig, ToolConfig, WinConfig, install_info, uninstall};

slint::include_modules!();

static LICENTEXT_GPL: &'static str = include_str!("../../../../LICENSE");
static WIN10: OnceLock<bool> = OnceLock::new();

fn main() -> Result<(), slint::PlatformError> {
  let mut backend = i_slint_backend_winit::Backend::new().unwrap();

  backend.window_attributes_hook = Some(Box::new(|b| {
    let mut out = b;
    _ = WIN10.set(if OsVersion::current() >= OsVersion::new(10, 0, 0, 22523) {
      out = out
        .with_transparent(true)
        .with_system_backdrop(BackdropType::MainWindow);

      // Use Win11 APIs
      false
    } else {
      out = out.with_transparent(false);
      // Use Win10 APIs (older Win11, or, Win10 detected)
      true
    });

    out
  }));

  slint::platform::set_platform(Box::new(backend)).unwrap();

  let licensegpl = LICENTEXT_GPL
    .lines()
    .map(SharedString::from)
    .collect::<Vec<_>>();

  let ui = MainWindow::new()?;

  if !args().any(|x| &*x == "--water") {
    let model = Rc::new(VecModel::from(licensegpl));
    ui.set_licensegpl(model.into());
  }

  if !*WIN10.get().unwrap() {
    let black_brush = Brush::SolidColor(Color::from_argb_u8(0, 0, 0, 0));

    ui.set_backg(black_brush);
    ui.set_win10(false);
  } else {
    let darkcolor = Brush::SolidColor(Color::from_rgb_u8(25, 20, 31));

    if ui.get_dark() {
      ui.set_backg(darkcolor);
    }
    ui.set_win10(true);
  }

  let fx = ui.as_weak();
  let fx2 = ui.as_weak();

  ui.on_startinstall(move |repair| {
    let fx = &fx;
    let fx2 = &fx2;

    let fx = fx.clone();
    let fx2 = fx2.clone();

    thread::spawn(move || {
      let fx = fx;
      let fx2 = fx2;

      let (tx, rx) = std::sync::mpsc::channel();
      fx.upgrade_in_event_loop(move |x| {
        _ = tx.send(x.get_configs().iter().collect::<Box<[bool]>>());
      })
      .unwrap();

      let configs: Box<[bool]> = rx.recv().expect("Sending unexpectedly dropped");

      let [
        staticarchives,
        linkstubs,
        cheaders,
        satest,
        saapprt,
        path,
        startmenu,
      ] = *configs
      else {
        unreachable!()
      };

      let config = Config {
        sdk: SDKConfig {
          headers: cheaders,
          linklibs: linkstubs,
          staticarchives,
        },
        tools: ToolConfig { saapprt, satest },
        w32: WinConfig {
          path,
          start: startmenu,
        },
      };

      #[cfg(debug_assertions)]
      println!("{config:?}");

      install_info::<_, _, true>(
        |tx, prog| {
          let fx = fx.clone();
          slint::invoke_from_event_loop(move || {
            if let Some(ui) = fx.upgrade() {
              ui.set_prog(prog as _);
              ui.set_status(SharedString::from(tx.as_ref()));
            }
          })
          .unwrap();
        },
        move || {
          slint::invoke_from_event_loop(move || {
            if let Some(ui) = fx2.upgrade() {
              ui.set_installed(true);
            }
          })
          .unwrap();
        },
        config,
        repair,
      );
    });
  });

  let fx = ui.as_weak();
  ui.on_startuninstall(move || {
    let fx2 = &fx;

    let fx = fx2.clone();
    thread::spawn(move || {
      uninstall::<_, true>(move || {
        slint::invoke_from_event_loop(move || {
          if let Some(ui) = fx.upgrade() {
            ui.set_uninstalled(true);
          }
        })
        .unwrap();
      });
    });
  });

  if let Some(_) = args().find(|x| x as &str == "uninstall") {
    ui.set_uninstall(true);
  }

  // Move to Repair Page
  if let Some(_) = args().find(|x| x as &str == "repair") {
    ui.set_curpage(Page::Repairconf);
  }

  ui.run()?;
  Ok(())
}

pub struct Sendable(MainWindow);
unsafe impl Send for Sendable {}
unsafe impl Sync for Sendable {}

impl Clone for Sendable {
  fn clone(&self) -> Self {
    Self(self.0.clone_strong())
  }
}
