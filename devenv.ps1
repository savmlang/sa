if ($true -ne (Test-Path "./llvm")) {
  Write-Error "LLVM is not installed - please run `./sacli` first to install it"
  
  exit 1
}

$env:LLVM_SYS_211_PREFIX = "$(Get-Location)$([IO.Path]::DirectorySeparatorChar)llvm$([IO.Path]::DirectorySeparatorChar)bin"

"✅ Your session has been loaded with SaVM Required Environment Variables"

exit 0