# Script to clean packages and temp files on a Windows machine
# not normally covered by cleaning programs

. $PSScriptRoot/common.ps1

Write-HostWithTimestamp "Cleaning packages and software projects"

# Check if scoop command exists, if so clean scoop packages
if (Test-Path ~\scoop\apps\scoop\current\bin\scoop.ps1) {
    Write-HostWithTimestamp "Cleaning scoop packages and cache"
    scoop cleanup *
    scoop cache rm --all
}

# Check if conda command exists, if so clean conda packages
if (Test-Path ~\scoop\apps\miniconda3\current\Scripts\conda.exe) {
    Write-HostWithTimestamp "Cleaning conda packages"
    conda clean --all --yes
}

# Clean mpv watch info
if (Test-Path ~\scoop\apps\mpv\current\portable_config\watch_later) {
    Write-HostWithTimestamp "Cleaning mpv watch info and cache"
    # delete watch data in scoop installed mpv
    Remove-Item -Path ~\scoop\apps\mpv\current\portable_config\watch_later\* -Force -Recurse
    Remove-Item -Path ~\scoop\apps\mpv\current\portable_config\cache\* -Force -Recurse
}

# Clean screenshots
if (Test-Path ~\Pictures\Screenshots) {
		Write-HostWithTimestamp "Cleaning screenshots"
		Remove-Item -Path ~\Pictures\Screenshots\* -Force -Recurse
}

# Clean Emacs Doom Packages
if (Test-Path ~\.config\emacs\bin\doom.cmd) {
  cd "~\.config\emacs\bin"
  doom purge
}

if (Test-Path ~\Code) {
		Write-HostWithTimestamp "Cleaning software projects unneed files"
		cd ~\Code
		kondo
}
