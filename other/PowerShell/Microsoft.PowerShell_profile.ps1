# PowerShell 7+ PROFILE
# Edit like notepad $PROFILE
# Stored in $PROFILE variable
# Managed by Ansible

# Variables
$env:EDITOR = "nvim"
## kubectl editor
$env:KUBE_EDITOR = "nvim"
## Yazi File Manager to open files on Windows https://yazi-rs.github.io/docs/installation
$env:YAZI_FILE_ONE = "$env:USERPROFILE\scoop\apps\git\current\usr\bin\file.exe"
## Set Lazyvim as default Neovim framework to use
$env:NVIM_APPNAME = "lazyvim"

# Prompt

## Starship
### Function for Starship to set Window title
### https://starship.rs/advanced-config/
function Invoke-Starship-PreCommand {
    $shortPath = $pwd -replace [regex]::Escape($HOME), '~'
    $host.ui.RawUI.WindowTitle = "$shortPath `a"
}

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
## Enable aliases for editing and process stop
Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t' -PSReadlineChordReverseHistory 'Ctrl+r' -EnableAliasFuzzyEdit -EnableAliasFuzzyKillProcess

# Intellisense

## Azure Predictor
### Import-Module Az.Tools.Predictor
### Enable-AzPredictor -AllSession

# Aliases

Set-Alias -Name k -Value kubectl
Set-Alias -Name lg -Value lazygit 

### Git Repositories checker
function jgt {
    gfold | rg -e unclean -e unpushed
}

### Git Repositories checker detailed
function jvcs {
    vcs status $env:USERPROFILE\Code | rg -e modified -e ===
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
        (& "$env:USERPROFILE\scoop\apps\miniconda3\current\Scripts\conda.exe" "shell.powershell" "hook") | Out-String | ? { $_ } | Invoke-Expression
        
        if ($Activate) {
            conda activate $Activate
        }
    }
}
#endregion

## Yazi File Manager
## Wrapper for yazi to allow change cwd when existing yazi
function y {
    $tmp = [System.IO.Path]::GetTempFileName()
    yazi $args --cwd-file="$tmp"
    $cwd = Get-Content -Path $tmp
    if (-not [String]::IsNullOrEmpty($cwd) -and $cwd -ne $PWD.Path) {
        Set-Location -LiteralPath $cwd
    }
    Remove-Item -Path $tmp
}

# Activate Experimental Atuin integration based on 
# https://github.com/rainersigwald/dotfiles/blob/main/Documents/PowerShell/Microsoft.PowerShell_profile.ps1 
function Use-Atuin {

    try {
        # If atuin is not available, stop. Supress output of check
        Get-Command atuin -ErrorAction Stop > $null 2>&1

        # Atuin must exist so register history via https://github.com/atuinsh/atuin/issues/84#issuecomment-2053600939
        $env:ATUIN_SESSION = (atuin uuid | Out-String).Trim()
        # Initialize the ATUIN_HISTORY_ID environment variable to null
        $env:ATUIN_HISTORY_ID = $null

        # Set a custom key handler for the Enter key to store command history in Atuin
        Set-PSReadLineKeyHandler -Chord Enter -ScriptBlock {
            $line = $null
            $cursor = $null
            # Get current input and the position of the cursor
            [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$line, [ref]$cursor)

            # If ATUIN_HISTORY_ID is not set, get a history ID
            if (-not $env:ATUIN_HISTORY_ID) {
                $env:ATUIN_HISTORY_ID = (atuin history start -- $line | Out-String).Trim()
            }

            # Execute the input as normal
            [Microsoft.PowerShell.PSConsoleReadLine]::AcceptLine()
        }

        # Save the existing prompt function
        $existingPromptFunction = Get-Item -Path Function:\prompt
        # Remove the existing prompt function
        Remove-Item -Path Function:\prompt
        # Define a new prompt function
        function prompt {
            # If ATUIN_HISTORY_ID is set, end the Atuin history entry with duration and exit code
            if ($env:ATUIN_HISTORY_ID) {
                atuin history end --duration (Get-History -Count 1).Duration.TotalNanoseconds --exit $LASTEXITCODE -- $env:ATUIN_HISTORY_ID | Out-Null

                # Remove the ATUIN_HISTORY_ID environment variable
                Remove-Item -Path env:ATUIN_HISTORY_ID -ErrorAction SilentlyContinue
            }

            # Call the existing prompt function
            & $existingPromptFunction.ScriptBlock
        }
    
        # Ctrl + r to bring up atuin search of history and insert result for input
        # based on https://gist.github.com/lzybkr/b47b89e48a963429161836ca1443e8f5
        Set-PSReadLineKeyHandler -Key Ctrl+r -ScriptBlock {
            $line = $null
            $cursor = $null
            # Get the current command line buffer state
            [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$line, [ref]$cursor)
            # Create a temporary file to store the search result
            $resultFile = New-TemporaryFile
            # Start the Atuin search process and wait for it to complete
            Start-Process -Wait -NoNewWindow -RedirectStandardError $resultFile.FullName atuin -ArgumentList "search", "-i"
            # Read the search result from the temporary file
            $result = (Get-Content -Raw $resultFile).Trim()
            # Remove the temporary file
            Remove-Item $resultFile

            # Revert the current command line to its original state
            [Microsoft.PowerShell.PSConsoleReadLine]::RevertLine()
            # Insert the search result into the command line
            [Microsoft.PowerShell.PSConsoleReadLine]::Insert($result)
        }
    }
    catch {
        Write-Host "Atuin is not available. Skipping Atuin integration."
    }

}