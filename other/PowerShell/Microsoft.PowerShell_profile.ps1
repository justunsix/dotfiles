# PowerShell 7+ PROFILE
# Stored in $PROFILE variable
# Managed by Ansible
# Edit like notepad $PROFILE
Invoke-Expression (&starship init powershell)

# Zoxide
Invoke-Expression (& {
    $hook = if ($PSVersionTable.PSVersion.Major -lt 6) { 'prompt' } else { 'pwd' }
    (zoxide init --hook $hook powershell | Out-String)
})

# PSReadline
Import-Module PSReadLine
# Enable Predictive IntelliSense
Set-PSReadLineOption -PredictionSource History

# PSFzf 
Import-Module PSFzf
# Select Current Provider Path (default chord: Ctrl+t)
# Reverse Search Through PSReadline History (default chord: Ctrl+r)
# replace 'Ctrl+t' and 'Ctrl+r' with your preferred bindings:
Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t' -PSReadlineChordReverseHistory 'Ctrl+r'

# Intellisense 
## List view of predictions under the cursor
Set-PSReadLineOption -PredictionViewStyle ListView

## Azure Predictor
Import-Module Az.Tools.Predictor
Set-PSReadLineOption -PredictionSource HistoryAndPlugin
Enable-AzPredictor -AllSession
