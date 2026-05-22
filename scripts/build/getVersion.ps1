$filePath = Join-Path $PSScriptRoot "../../Cargo.toml"

$regex = 'metadata\.savm\.version\s*=\s*"v?([0-9]+\.[0-9]+\.[0-9]+(.*)?)"'

$match = Select-String -Path $filePath -Pattern $regex | Select-Object -First 1

if ($null -eq $match) {
    throw "Version could not be found."
}

Write-Output $match.Matches.Groups[1].Value
