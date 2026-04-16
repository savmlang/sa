if ($true -ne (Test-Path -Path "./scripts/cli/node_modules")) {
  "🏗️  Preparing SaCLI"
  
  Set-Location ./scripts/cli
  npm i
  Set-Location ../../

  "✅ SaCLI is booting up"
  ""
}

node ./scripts/cli/src/index.js @args