$tgt = $env:TARGET

if (Test-Path "./target/$tgt/release") {
  Get-ChildItem -Path "./target/$tgt/release/*" | Where-Object { ! $_.PSIsContainer } | Compress-Archive -Update -DestinationPath "$env:PACKAGE.zip"
}