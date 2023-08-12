# Script to configure a new Windows desktop
# Run under regular Windows PowerShell

# Execute in PowerShell steps that are 
# automated by Ansible at windows.yml 
# at https://github.com/justunsix/dotfiles-playbook

# Assuming Scoop is installed, install packages
jt-work-install-scoop.ps1
# Assuming Chocolatey is installed, install packages
gsudo jt-work-install-choco.ps1

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
gsudo choco upgrade all -y

# Install PowerShell 7+ modules
pwsh

## Azure Predictor Intellisense - pwsh 7+
Install-Module -Name Az.Accounts -Force
Install-Module -Name Az.Tools.Predictor -Force
## CompletionPredictor - pwsh 7+
Install-Module -Name CompletionPredictor

# Synchronize dotfiles
jt-work-dotfiles.ps1