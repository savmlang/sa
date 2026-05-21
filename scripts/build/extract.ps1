Remove-Item -Path "./outputs/" -Recurse -ErrorAction SilentlyContinue
New-Item -Path "./outputs/" -ItemType Directory

Remove-Item -Path "./extracted/" -Recurse -ErrorAction SilentlyContinue
New-Item -Path "./extracted/" -ItemType Directory

foreach ($tech in Get-ChildItem "./release" -Directory) {
  $name = $tech.Name;

  if (Test-Path "./release/${name}/${name}.zip") {
    # Zip
    New-Item -Path "./extracted/${name}" -ItemType Directory
    Expand-Archive -Path "./release/${name}/${name}.zip" -DestinationPath "./extracted/${name}/"
  }
  else {
    # Output Artifact
    Get-ChildItem -Path $tech.FullName -File -Recurse | Copy-Item -Destination "./outputs/" -Force
  }
}