# Script to clean packages and temp files on a Windows machine
# not normally covered by cleaning programs

. $PSScriptRoot/common.ps1

Write-HostWithTimestamp "Cleaning packages and software projects"

# Clean Emacs and Doom Packages
if (Test-Path ~\.config\emacs\.local\cache) {
    Write-HostWithTimestamp "Cleaning Emacs non-essential cache"
    Remove-Item -Path ~\.config\emacs\.local\cache\autosave -Force -Recurse
    Remove-Item -Path ~\.config\emacs\.local\cache\org -Force -Recurse
    Remove-Item -Path ~\.config\emacs\.local\cache\undo-fu-session -Force -Recurse
}

if (Test-Path ~\.config\emacs\bin\doom.cmd) {
    Write-HostWithTimestamp "Cleaning Doom Emacs Packages and complete Emacs Cache"
    Write-Host "Warning: Doom clean takes a while, cleaning Emacs cache will remove saved project, recent and files history"
    $confirmation = Read-Host "Continue? (y/n)"

    if ($confirmation -eq 'y' -or $confirmation -eq 'Y') {
        Write-HostWithTimestamp "Cleaning Doom Emacs Packages"
        cd "~\.config\emacs\bin"
        .\doom gc 

        if (Test-Path ~\.config\emacs\.local\cache) {
            Write-HostWithTimestamp "Cleaning Emacs cache"
            Remove-Item -Path ~\.config\emacs\.local\cache\* -Force -Recurse
        }
    } else {
        Write-HostWithTimestamp "Skipped cleaning Emacs."
    }
}

if (Test-Path ~\Code) {
    Write-HostWithTimestamp "Cleaning software projects unneeded files"
    cd ~\Code
    kondo
}

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