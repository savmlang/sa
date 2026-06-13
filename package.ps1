$tgt = $env:TARGET

New-Item -Path "./target/c/include" -ItemType Directory -ErrorAction SilentlyContinue

$crates = @(
  "savmc", "sasmc", "salloc"
)

foreach ($crate in $crates) {
  cbindgen --config cbindgen.toml --crate $crate --output ./target/c/include/$crate.h
}

if (Test-Path "./target/$tgt/release") {
  (Get-ChildItem -File -Path "./target/$tgt/release/*"), (Get-ChildItem -Recurse -Path "./target/c/") | 
  Compress-Archive -Update -DestinationPath "$env:PACKAGE.zip"
}