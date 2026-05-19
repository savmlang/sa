Remove-Item -Path "./extracted/" -Recurse -ErrorAction SilentlyContinue
New-Item -Path "./extracted/" -ItemType Directory

foreach ($tech in Get-ChildItem "./release" -Directory) {
  $name = $tech.Name;
  
  New-Item -Path "./extracted/${name}" -ItemType Directory
  Expand-Archive -Path "./release/${name}/${name}.zip" -DestinationPath "./extracted/${name}/"
}