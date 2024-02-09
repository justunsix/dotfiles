# PowerShell 7+ PROFILE
# Edit like notepad $PROFILE
# Stored in $PROFILE variable
# Managed by Ansible

# Prompt

## Starship
if (Get-Command "starship.exe" -ErrorAction SilentlyContinue)
{
		Invoke-Expression (&starship init powershell)
}

# Shell Support
## Zoxide
if (Get-Command "zoxide.exe" -ErrorAction SilentlyContinue)
{
		Invoke-Expression (& {
													 $hook = if ($PSVersionTable.PSVersion.Major -lt 6) { 'prompt' } else { 'pwd' }
													 (zoxide init --hook $hook powershell | Out-String)
											 })
}

## PSReadline
Import-Module PSReadLine
## Enable Predictive IntelliSense
Set-PSReadLineOption -PredictionSource History

## PSFzf
Import-Module PSFzf
## Select Current Provider Path (default chord: Ctrl+t)
## Reverse Search Through PSReadline History (default chord: Ctrl+r)
## replace 'Ctrl+t' and 'Ctrl+r' with your preferred bindings:
Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t' -PSReadlineChordReverseHistory 'Ctrl+r'

## Intellisense
Import-Module CompletionPredictor
### List view of predictions under the cursor
Set-PSReadLineOption -PredictionViewStyle ListView

## Azure Predictor
Import-Module Az.Tools.Predictor
Set-PSReadLineOption -PredictionSource HistoryAndPlugin
Enable-AzPredictor -AllSession

# Aliases
## Emacs
## Uses function instead of alias
## per https://stackoverflow.com/questions/4166370/how-can-i-write-a-powershell-alias-with-arguments-in-the-middle
## function emacsnw {emacs -Q -nw -l ~\.config\emacs\setup\minimal.el $args}

# Integrated Programs

## Broot
if (Get-Command "broot.exe" -ErrorAction SilentlyContinue)
{
		if (Test-Path $env:USERPROFILE\AppData\Roaming\dystroy\broot\config\launcher\powershell\br.ps1) {
				. $env:USERPROFILE\AppData\Roaming\dystroy\broot\config\launcher\powershell\br.ps1
		}
}
