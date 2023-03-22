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
azure-cli `
drawio `
nodejs `
python `
maven `
mremoteng `
pdfxchangeeditor `
zoxide `
plantuml `
fd `
-y

# Install power-core with arguments per 
# https://community.chocolatey.org/packages/powershell-core/
choco install powershell-core -y --install-arguments='""ADD_FILE_CONTEXT_MENU_RUNPOWERSHELL=1 ADD_EXPLORER_CONTEXT_MENU_OPENPOWERSHELL=1 REGISTER_MANIFEST=1 ENABLE_PSREMOTING=1"'

# Alternatively for powershell-core, install using winget to avoid powershell upgrade issues
# winget install --id Microsoft.Powershell --source winget

# Upgrade all installed packages
choco upgrade all -y
