# PowerShell 7+ PROFILE
# Edit like notepad $PROFILE
# Stored in $PROFILE variable
# Managed by Ansible

# Variables
$env:EDITOR = "nvim"
## kubectl editor
$env:KUBE_EDITOR="nvim"
## Yazi File Manager to open files on Windows https://yazi-rs.github.io/docs/installation
$env:YAZI_FILE_ONE = "$env:USERPROFILE\scoop\apps\git\current\usr\bin\file.exe"
## Set Lazyvim as default Neovim framework to use
$env:NVIM_APPNAME = "lazyvim"

# Prompt

## Starship
if (Get-Command "starship.exe" -ErrorAction SilentlyContinue) {
	Invoke-Expression (&starship init powershell)
}

# Shell Support
## PSReadline
Import-Module PSReadLine
Import-Module CompletionPredictor
Set-PSReadLineOption -PredictionSource HistoryAndPlugin
### List view of predictions under the cursor
### Set-PSReadLineOption -PredictionViewStyle ListView

## Zoxide
if (Get-Command "zoxide.exe" -ErrorAction SilentlyContinue) {
	Invoke-Expression (& {
			$hook = if ($PSVersionTable.PSVersion.Major -lt 6) { 'prompt' } else { 'pwd' }
													 (zoxide init --hook $hook powershell | Out-String)
		})
}

## Carapace - Shell completions
if (Get-Command "carapace.exe" -ErrorAction SilentlyContinue) {
	$env:CARAPACE_BRIDGES = 'zsh,fish,bash,inshellisense' # optional
	Set-PSReadLineOption -Colors @{ "Selection" = "`e[7m" }
	Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete
	carapace _carapace | Out-String | Invoke-Expression
}

## Broot - File explorer, directory navigator
if (Get-Command "broot.exe" -ErrorAction SilentlyContinue) {
	if (Test-Path $env:USERPROFILE\AppData\Roaming\dystroy\broot\config\launcher\powershell\br.ps1) {
		. $env:USERPROFILE\AppData\Roaming\dystroy\broot\config\launcher\powershell\br.ps1
	}
}

## PSFzf
## https://github.com/kelleyma49/PSFzf
Import-Module PSFzf
## Select Current Provider Path (default chord: Ctrl+t)
## Reverse Search Through PSReadline History (default chord: Ctrl+r)
## replace 'Ctrl+t' and 'Ctrl+r' with your preferred bindings:
Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t' -PSReadlineChordReverseHistory 'Ctrl+r'

# Intellisense

## Azure Predictor
### Import-Module Az.Tools.Predictor
### Enable-AzPredictor -AllSession

# Aliases

### Git Repositories checker
function jgt {
		gfold | rg -e unclean -e unpushed
}

### Git Repositories checker detailed
function jvcs {
		vcs status $env:USERPROFILE\Code | rg -e modified -e ===
}

### Git Repositories checker detailed
function lg {
		lazygit
}

### eza
function e {
		eza -alh --git
}

## Emacs
## Uses function instead of alias
## per https://stackoverflow.com/questions/4166370/how-can-i-write-a-powershell-alias-with-arguments-in-the-middle
## function emacsnw {emacs -Q -nw -l ~\.config\emacs\setup\minimal.el $args}

# Functions

# region conda initialize
# source from https://github.com/conda/conda/issues/11648 to only init conda on use
# Also run conda config --set auto_activate_base false to disable auto-activation
# Usage: Use-Conda <conda_environment_name>
## !! Contents within this block are managed by 'conda init' !!
## Updated to us $env variable instead of hardcoded path
function Use-Conda {
    param (
        [string] $Activate
    )
    
    If (Test-Path "$env:USERPROFILE\scoop\apps\miniconda3\current\Scripts\conda.exe") {
        (& "$env:USERPROFILE\scoop\apps\miniconda3\current\Scripts\conda.exe" "shell.powershell" "hook") | Out-String | ?{$_} | Invoke-Expression
        
        if ($Activate) {
            conda activate $Activate
        }
    }
}
#endregion
