# Install with Chocolatey the following packages
# List package on each line by itself
choco install `
microsoft-windows-terminal `
autohotkey `
git `
git-credential-manager-for-windows `
starship `
fzf  `
python3 `
firefox `
jetbrainsmono `
peazip `
vlc `
veracrypt `
keepassxc `
cygwin `
vscode `
emacs `
ripgrep `
hunspell.portable `
pandoc `
capture2text `
inkscape `
gimp `
virtualbox `
zoom `
azure-cli `
drawio `
nodejs `
python `
maven `
mremoteng `
pdfxchangeeditor `
-y

# Upgrade all installed packages
choco upgrade all -y
