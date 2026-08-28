$iter = Get-ChildItem "./extracted" -Directory

$curr = 0
$total = $iter.Length

$nfpmpkgsgnu = "archlinux", "deb", "rpm", "srpm"
$nfpmpkgsmusl = "apk", "ipk"

$env:SAVM_VERSION = $(./getVersion.ps1)

if (Test-Path "./nfpm/key.gpg") {
  Set-Location nfpm

  npm i -g pnpm
  pnpm install
  node nfpm.setup.js

  Set-Location ..
}

foreach ($tech in $iter) {
  # AutoSet for future rust compiles
  $env:TARGET_PKG_DIR = $tech.FullName
  $env:TARGET_PKG_ZIP = Resolve-Path ("./release/" + $tech.BaseName + "/" + $tech.Name + ".zip")

  $dirn = $tech.Name

  $curr += 1
  $splits = $tech.Name.Split("-")

  $env:EDITION = $splits[-1]
  $rustarch = $splits[1]

  # If Linux (+ linbuild enabled)
  if ($tech.Name.Contains("-linux-").Equals($true)) {
    if ($env:LINBUILD -eq "true") {
      switch ($rustarch) {
        "armv7" {
          $env:BUNDLE_ARCH = "arm7"
        }
        "aarch64" {
          $env:BUNDLE_ARCH = "arm64"
        }
        "i686" {
          $env:BUNDLE_ARCH = "386"
        }
        "powerpc64le" {
          $env:BUNDLE_ARCH = "ppc64le"
        }
        "riscv64gc" {
          $env:BUNDLE_ARCH = "riscv64"
        }
        "x86_64" {
          $env:BUNDLE_ARCH = "amd64"
        }
        Default {
          Write-Error "Unable to find a supported architecture for $rustarch"
          exit 1
        }
      }

      if ($env:BUNDLE_ARCH -ne "") {
        Write-Host "(${curr}/${total}) Building nFPM ($dirn)...."

        # Move to nFPM to do the builds
        Set-Location nfpm
      
        node nfpm.recipe.js
        $env:SAVM_EXPANDED = "../extracted/$dirn"

        if ($dirn.Contains("gnu").Equals($true)) {
          $packgers = $nfpmpkgsgnu
        }
        else {
          $packgers = $nfpmpkgsmusl
        }

        $packgers | ForEach-Object -Parallel {
          $output = nfpm pkg -p $_ --target ../outputs/ 2>&1
          $output

          if ($LASTEXITCODE -ne 0) {
            throw "nfpm failed for package: $_"
          }
        }
        
        Set-Location ..
      }
    }
    else {
      Write-Host "(${curr}/${total}) Ignored Linux ($dirn)"
    }
  }
  

  # If Windows (+ windows device)
  if ($tech.Name.Contains("-pc-windows-").Equals($true)) {
    if ($IsWindows) {
      Write-Host "(${curr}/${total}) Building Windows ($dirn)"

      Set-Location win32

      $target = "$rustarch-pc-windows-msvc"
      rustup target add $target

      if ($env:DEBUG -eq "true") {
        cargo build --bins --target $target  
      } else {
        cargo build --bins --release --target $target
      }

      Set-Location ..

      Copy-Item ./target/$target/release/savm_windows_cli.exe -Destination ./outputs/savm_${env:EDITION}_windows_cli_${rustarch}_setup.exe
      Copy-Item ./target/$target/release/savm_windows_gui.exe -Destination ./outputs/savm_${env:EDITION}_windows_gui_${rustarch}_setup.exe
    }
    else {
      Write-Host "(${curr}/${total}) Ignored Windows ($dirn)"
    }
  }
  

  # If macOS + mac device
  if ($tech.Name.Contains("-apple-darwin").Equals($true)) {
    if ($IsMacOS) {
      Write-Host "(${curr}/${total}) Building macOS ($dirn)"

      Set-Location macos

      $target = "$rustarch-apple-darwin"
      rustup target add $target
      cargo build --release --target $target

      Set-Location ..

      Copy-Item ./target/$target/release/savm_macos -Destination ./outputs/savm_${env:EDITION}_macos_${rustarch}_setup
    }
    else {
      Write-Host "(${curr}/${total}) Ignored macOS ($dirn)"
    }
  }
}
