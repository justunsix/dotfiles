# Script to update all packages on a Windows machine

# Update with topgrade
# disable system updates on Windows for now
topgrade -y --disable system

Write-Host "`nCleaning scoop packages" -ForegroundColor Green
scoop cleanup *

Write-Host "`nUpdating Emacs packages" -ForegroundColor Green
emacs --batch --eval '(progn (package-refresh-contents) (package-upgrade-all))'
emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(straight-pull-all)'