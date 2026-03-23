$sourceFile = "0.sasm"
$iterations = 10000

Write-Host "Cloning $sourceFile $iterations times..." -ForegroundColor Cyan

foreach ($i in 1..$iterations) {
  $newName = "./bin/$i.sasm"
  
  Copy-Item -Path $sourceFile -Destination $newName
}

Write-Host "Done! Your directory is now heavy." -ForegroundColor Green