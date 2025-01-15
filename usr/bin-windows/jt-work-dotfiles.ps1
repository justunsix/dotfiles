# Script to synchronize dotfiles periodically

. $PSScriptRoot/common.ps1

#############
# Functions #
#############

# Function to copy source path to destination path
# Path can be a file or directory
# If directory, copy recursively
function Copy-SourceToDestination {
  param(
    [Parameter(Mandatory = $true)]
    [string]$source,
    [Parameter(Mandatory = $true)]
    [string]$destination
  )
  if (Test-Path $source) {
    if (Test-Path $destination) {
      Write-Host "+ Copying $source to $destination"
      Copy-Item -Recurse -Path $source -Destination $destination -Force
    }
    else {
      Write-Host "+ Copying $source to $destination"
      Copy-Item -Recurse -Path $source -Destination $destination
    }
  }
  else {
    Write-Host "= Could not find $source"
  }
}

#############
# Variables #
#############

# Array of dotfiles to remove
$dotfiles_to_be_removed = @(
  "$env:USERPROFILE\AppData\Local\topgrade.toml",
  "$env:USERPROFILE\.emacs",
  "$env:USERPROFILE\.emacs.d"
)

$dotfiles_directories = @(
  "$env:USERPROFILE\.config\todotxt-cli",
  "$env:USERPROFILE\.config\wezterm",
  "$env:USERPROFILE\AppData\Roaming\dystroy\broot\config",
  "$env:USERPROFILE\AppData\Roaming\helix",
  "$env:USERPROFILE\AppData\Roaming\navi",
  "$env:USERPROFILE\AppData\Roaming\nushell",
  "$env:USERPROFILE\.config\",
  "$env:USERPROFILE\.config\doom",
  "$env:USERPROFILE\.config\emacs",
  "$env:USERPROFILE\.config\fish"
  "$env:USERPROFILE\AppData\Local\lazyvim",
  "$env:USERPROFILE\AppData\Local\nvim",
  "$env:USERPROFILE\AppData\Roaming\alacritty",
  "$env:USERPROFILE\AppData\Roaming\topgrade",
  "$env:USERPROFILE\AppData\Roaming\yazi\config",
  "$env:USERPROFILE\usr\bin"
)

# Array of dotfiles to synchronize with tuples of source and destination
$dotfiles_to_be_synchronized = @(
  # Reserve for Doom Emacs install
		# ("$env:USERPROFILE\Code\dotfiles\.config\emacs", "$env:USERPROFILE\.config"),
		("$env:USERPROFILE\Code\dotfiles\.config\alacritty", "$env:USERPROFILE\AppData\Roaming"),
		("$env:USERPROFILE\Code\dotfiles\.config\fish", "$env:USERPROFILE\.config"),
		("$env:USERPROFILE\Code\dotfiles\.config\lazyvim", "$env:USERPROFILE\AppData\Local"),
		("$env:USERPROFILE\Code\dotfiles\.config\nvim", "$env:USERPROFILE\AppData\Local"),
		("$env:USERPROFILE\Code\dotfiles\.config\starship.toml", "$env:USERPROFILE\.config"),
		("$env:USERPROFILE\Code\dotfiles\.config\topgrade", "$env:USERPROFILE\AppData\Roaming"),
		("$env:USERPROFILE\Code\dotfiles\.gitconfig", "$env:USERPROFILE"),
		("$env:USERPROFILE\Code\dotfiles\usr\bin-windows\*", "$env:USERPROFILE\usr\bin"),
    ("$env:USERPROFILE\Code\dotfiles\.config\broot\*", "$env:USERPROFILE\AppData\Roaming\dystroy\broot\config"),
    ("$env:USERPROFILE\Code\dotfiles\.config\dolphinrc", "$env:USERPROFILE\AppData\Local"),
    ("$env:USERPROFILE\Code\dotfiles\.config\doom", "$env:USERPROFILE\.config"),
    ("$env:USERPROFILE\Code\dotfiles\.config\gfold.toml", "$env:USERPROFILE\.config"),
    ("$env:USERPROFILE\Code\dotfiles\.config\helix", "$env:USERPROFILE\AppData\Roaming"),
    ("$env:USERPROFILE\Code\dotfiles\.config\kdeglobals", "$env:USERPROFILE\AppData\Local"),
    ("$env:USERPROFILE\Code\dotfiles\.config\navi", "$env:USERPROFILE\AppData\Roaming"),
    ("$env:USERPROFILE\Code\dotfiles\.config\nushell", "$env:USERPROFILE\AppData\Roaming"),
    ("$env:USERPROFILE\Code\dotfiles\.config\todotxt-cli", "$env:USERPROFILE\.config"),
    ("$env:USERPROFILE\Code\dotfiles\.config\wezterm", "$env:USERPROFILE\.config"),
    ("$env:USERPROFILE\Code\dotfiles\.config\yazi\*", "$env:USERPROFILE\AppData\Roaming\yazi\config"),
    ("$env:USERPROFILE\Code\dotfiles\other\WindowsPowerShell\Microsoft.PowerShell_profile.ps1", "$PROFILE")
)

###############
# Main Script #
###############

# For each item in $dotfiles_to_be_removed, remove file if it exists
Write-HostWithTimestamp "Removing dotfiles not required"
foreach ($dotfile in $dotfiles_to_be_removed) {
  if (Test-Path $dotfile) {
    Write-Host "- Removing $dotfile"
    Remove-Item -Path $dotfile -Force
  }
  else {
    Write-Host "= Could not find $dotfile : skipping"
  }
}

# Create directories if they do not exist already
Write-HostWithTimestamp "Creating dotfiles directories if they do not exist"
foreach ($directory in $dotfiles_directories) {
  if (Test-Path $directory) {
    Write-Host "= Found $directory"
  }
  else {
    Write-Host "+ Creating $directory"
    New-Item -Path $directory -ItemType Directory
  }
}

# Synchronize Dotfiles
# For each tuple in $dotfiles_to_be_synchronized, copy source to destination
Write-HostWithTimestamp "Synchronizing dotfiles"
foreach ($dotfile in $dotfiles_to_be_synchronized) {
  Copy-SourceToDestination -source $dotfile[0] -destination $dotfile[1]
}

# In .config/gfold.toml, replace string /home/justin/Code with the value of $env:USERPROFILE\Code
$gfold_toml = "$env:USERPROFILE\.config\gfold.toml"
$gfold_toml_windows_path = "$env:USERPROFILE\Code"
if (Test-Path $gfold_toml) {
  Write-Host "+c Replacing /home/justin/Code with $gfold_toml_windows_path in $gfold_toml"
  (Get-Content $gfold_toml) | ForEach-Object { $_ -replace "/home/justin/Code", "$gfold_toml_windows_path" } | Set-Content $gfold_toml
}
else {
  Write-Host "= Could not find $gfold_toml"
}

# In kdeglobals, remove lines [Icons] and Theme=breeze-dark
# to fix issue on Windows where icons will appear dark foreground on dark background
$kdeglobals = "$env:USERPROFILE\AppData\Local\kdeglobals"
if (Test-Path $kdeglobals) {
  Write-Host "+c Removing [Icons] and Theme=breeze-dark from $kdeglobals"
  (Get-Content $kdeglobals) | ForEach-Object { $_ -replace "\[Icons\]", "" } | Set-Content $kdeglobals
  (Get-Content $kdeglobals) | ForEach-Object { $_ -replace "Theme=breeze-dark", "" } | Set-Content $kdeglobals
}
else {
  Write-Host "= Could not find $kdeglobals"
}

# doom config, replace line +roam2 with ;; +roam2
$doom_config = "$env:USERPROFILE\.config\doom\init.el"
if (Test-Path $doom_config) {
  Write-Host "+c Replacing +roam2 with ;; +roam2 in $doom_config"
  (Get-Content $doom_config) | ForEach-Object { $_ -replace "\+roam2", ";; +roam2" } | Set-Content $doom_config
}
else {
  Write-Host "= Could not find $doom_config"
}

# navi config, replace bash with cmd.exe for shell
$navi_config = "$env:USERPROFILE\AppData\Roaming\navi\config.yaml"
if (Test-Path $navi_config) {
  Write-Host "+c Replacing bash with cmd.exe in $navi_config"
  (Get-Content $navi_config) | ForEach-Object { $_ -replace "bash", "cmd.exe" } | Set-Content $navi_config
}
else {
  Write-Host "= Could not find $navi_config"
}

# Lazyvim extra config, remove lang.nix
$lazyvim_extras_config = "$env:USERPROFILE\AppData\Local\lazyvim\lazyvim.json"
if (Test-Path $lazyvim_extras_config) {
  Write-Host "+c removing lang.nix in $lazyvim_extras_config" 
  (Get-Content $lazyvim_extras_config) | Where-Object { $_ -notmatch 'extras\.lang\.nix' } | Set-Content $lazyvim_extras_config
}
else {
  Write-Host "= Could not find $lazyvim_extras_config"
}

# Disable auto-activation of base conda environment
conda config --set auto_activate_base false
