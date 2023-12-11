# Script to update all packages on a Windows machine

# Update with topgrade
# disable system updates on Windows for now
# Prompt user whether to run topgrade
$runTopgrade = Read-Host "Do you want to run topgrade? (y/n)"

# If user wants to run topgrade, then run topgrade with options
if ($runTopgrade -eq "y") {    
    $runTopgradeWithPS1 = Read-Host "Do you want to run topgrade with powershell? (y/n)"
    Write-Host "`nRunning topgrade" -ForegroundColor Green

    if ($runTopgradeWithPS1 -eq "y") {
        topgrade -y --disable system
    } else {
        Write-Host "`nwith --disable powershell" -ForegroundColor Green
        topgrade -y --disable system --disable powershell
    }
}

Write-Host "`nCleaning scoop, conda  packages" -ForegroundColor Green
scoop cleanup *
conda clean --all --yes

Write-Host "`nCleaning mpv watch info" -ForegroundColor Green
# delete watch data in scoop installed mpv
Remove-Item -Path ~\scoop\apps\mpv\current\portable_config\watch_later\* -Force -Recurse

Write-Host "`nUpdating Emacs packages" -ForegroundColor Green
# emacs --batch --eval '(progn (package-refresh-contents) (package-upgrade-all))'
emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(auto-package-update-now)'
emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(straight-pull-all)'
