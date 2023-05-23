# Install with Chocolatey and PowerShell the following packages
# List package on each line by itself
choco install `
microsoft-windows-terminal `
autohotkey `
git `
git-credential-manager-for-windows `
python3 `
jetbrainsmono `
veracrypt `
cygwin `
hunspell.portable `
virtualbox `
azure-cli `
nodejs `
python `
obs-studio `
-y

# Chocolatey packages moved to scoop install
# firefox `
# capture2text `
# inkscape `
# gimp `
# starship `
# fzf  `
# peazip `
# vlc `
# keepassxc `
# vscode `
# emacs `
# ripgrep `
# pandoc `
# drawio `
# maven `
# mremoteng `
# zoxide `
# plantuml `
# fd `
# terraform `

# Install PowerShell-core with arguments per
# https://community.chocolatey.org/packages/powershell-core/
choco install powershell-core -y --install-arguments='""ADD_FILE_CONTEXT_MENU_RUNPOWERSHELL=1 ADD_EXPLORER_CONTEXT_MENU_OPENPOWERSHELL=1 REGISTER_MANIFEST=1 ENABLE_PSREMOTING=1"'

# Alternatively for powershell-core, install using winget to avoid powershell upgrade issues
# winget install --id Microsoft.Powershell --source winget

# Install Readline supports
Install-Module -Name PSReadline -AllowClobber -Force
Install-Module -Name PSFzf
# Install Azure tools
## Azure Active Directory, deprecated 2023-06
Install-Module -Name AzureADPreview

# Upgrade all installed packages
choco upgrade all -y

# Install PowerShell 7+ modules
pwsh

## Azure Predictor Intellisense - pwsh 7+
Install-Module -Name Az.Accounts -Force
Install-Module -Name Az.Tools.Predictor -Force
## CompletionPredictor - pwsh 7+
Install-Module -Name CompletionPredictor
