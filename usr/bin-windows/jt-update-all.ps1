# Script to update all packages on a Windows machine

# Update with topgrade
# disable system updates on Windows for now
# Prompt user whether to run topgrade
$runTopgrade = Read-Host "Do you want to run topgrade? (y/n)"

# If user wants to run topgrade, then run topgrade with options
if ($runTopgrade -eq "y") {
    $runTopgradeWithPS1 = Read-Host "Do you want to run topgrade with powershell and vagrant? (y/n)"
    Write-Host "`nRunning topgrade" -ForegroundColor Green

    if ($runTopgradeWithPS1 -eq "y") {
        topgrade -y --disable system
    } else {
        Write-Host "`nwith --disable powershell vagrant" -ForegroundColor Green
        topgrade -y --disable system --disable powershell vagrant
    }
}

Write-Host "`nCleaning packages" -ForegroundColor Green

# Check if scoop command exists, if so update scoop
if (Test-Path ~\scoop\apps\scoop\current\bin\scoop.ps1) {
    Write-Host "`nCleaning scoop packages and cache" -ForegroundColor Green
    scoop cleanup *
    scoop cache rm --all
}

# Check if conda command exists, if so update conda
if (Test-Path ~\scoop\apps\miniconda3\current\Scripts\conda.exe) {
    Write-Host "`nCleaning conda packages" -ForegroundColor Green
    conda clean --all --yes
}

# Check if directory "~\scoop\apps\mpv\current\portable_config\watch_later" exists, if so clean
if (Test-Path ~\scoop\apps\mpv\current\portable_config\watch_later) {
    Write-Host "`nCleaning mpv watch info" -ForegroundColor Green
    # delete watch data in scoop installed mpv
    Remove-Item -Path ~\scoop\apps\mpv\current\portable_config\watch_later\* -Force -Recurse
}

# Check if emacs command exists, if so update packages
if (Test-Path ~\scoop\apps\emacs\current\bin\emacs.exe) {
    Write-Host "`nUpdating Emacs packages" -ForegroundColor Green
    # emacs --batch --eval '(progn (package-refresh-contents) (package-upgrade-all))'
    emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(auto-package-update-now)'
    emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(straight-pull-all)'
}
