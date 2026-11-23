$tgt = $env:TARGET

New-Item -Path "./target/c/include" -ItemType Directory -ErrorAction SilentlyContinue

$crates = @(
  "savmc", "sasmc", "salloc"
)

foreach ($crate in $crates) {
  cbindgen --config cbindgen.toml --crate $crate --output ./target/c/include/$crate.h
}

if ([System.OperatingSystem]::IsWindows()) {
  ./installers/win32_loadsign.ps1

  $signable = @(
    "*.dll",
    "*.exe"
  )

  $items = Get-ChildItem "./target/$tgt/release/*" -Include $signable -File

  foreach ($item in $items) {
    $env:BINARY = $item.FullName
    ./installers/win32_sign.ps1
  }
}

if (Test-Path "./target/$tgt/release") {
  $cleanup = @(
    # Link stubs and export files (Windows / Apple linker artifacts)
    "*.dll.exp",
    "*.exp",
    "*.tbd",

    # Build metadata & dependency graphs
    "*.d",
    ".cargo-artifact-lock",
    ".cargo-build-lock",
    ".cargo-lock",

    # Debug symbols
    "*.pdb",
    "*.dSYM",
    "*.dbg",
    "*.debug",

    # Rust libraries
    "*.rlib"
  )

  # Clean target directory safely
  Get-ChildItem -Path "./target/$tgt/release/*" -Include $cleanup | 
  Remove-Item -Recurse -Force -ErrorAction SilentlyContinue

  (Get-ChildItem -File -Path "./target/$tgt/release/*"), (Get-ChildItem -Recurse -Path "./target/c/") | 
  Compress-Archive -Update -DestinationPath "$env:PACKAGE.zip"
}

Set-Location installers

New-Item -Path "./dist" -ItemType Directory -ErrorAction SilentlyContinue

if ([System.OperatingSystem]::IsWindows()) {
  ./win32.ps1
}

if ([System.OperatingSystem]::IsLinux()) {
  ./linux.ps1
}

if ([System.OperatingSystem]::IsMacOS()) {
  ./macos.ps1
}

Set-Location ..