use tauri_winres::WindowsResource;

fn main() {
  println!("cargo:rerun-if-env-changed=TARGET_PKG_DIR");

  slint_build::compile("ui/index.slint").expect("Slint build failed");

  let mut res = WindowsResource::new();
  res.set_icon("./assets/salang.ico");
  res.set_manifest(
    r#"
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
<trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">
    <security>
        <requestedPrivileges>
            <requestedExecutionLevel level="requireAdministrator" uiAccess="false" />
        </requestedPrivileges>
    </security>
</trustInfo>
</assembly>
"#,
  );
  res
    .compile_for(&["savm_windows_gui", "savm_windows_cli"])
    .unwrap();
}
