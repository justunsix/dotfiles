# Script to clean packages and temp files on a Windows machine
# not normally covered by cleaning programs

. $PSScriptRoot/common.ps1

# Update with topgrade
# disable system updates on Windows for now
# Prompt user whether to run topgrade
Write-HostWithTimestamp "Topgrade"
$runTopgrade = Read-Host "Do you want to run topgrade? (y/n)"

# If user wants to run topgrade, then run topgrade with options
if ($runTopgrade -eq "y") {
    $runTopgradeWithPS1 = Read-Host "Do you want to run topgrade with powershell, emacs, vim, and vagrant? (y/n)"
    Write-Host "`nRunning topgrade" -ForegroundColor Green

		# Disable vscode due to bug for now
    if ($runTopgradeWithPS1 -eq "y") {
        topgrade -y --disable system vscode
    } else {
        Write-Host "`nwith --disable powershell vagrant emacs vim" -ForegroundColor Green
        topgrade -y --disable system --disable powershell vagrant emacs vim vscode
    }
}

# Check if emacs command exists, if so update packages
if (Test-Path ~\scoop\apps\emacs\current\bin\emacs.exe) {
    Write-HostWithTimestamp "Updating Emacs packages"
    emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(auto-package-update-now)'
    emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(straight-pull-all)'
}

jt-clean-up.ps1