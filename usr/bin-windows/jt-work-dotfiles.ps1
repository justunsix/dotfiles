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
    [Parameter(Mandatory=$true)]
    [string]$source,
    [Parameter(Mandatory=$true)]
    [string]$destination
  )
  if (Test-Path $source) {
    if (Test-Path $destination) {
      Write-Host "+ Copying $source to $destination"
      Copy-Item -Recurse -Path $source -Destination $destination -Force
    } else {
      Write-Host "+ Copying $source to $destination"
      Copy-Item -Recurse -Path $source -Destination $destination
    }
  } else {
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
  "$env:USERPROFILE\.config\",
  "$env:USERPROFILE\usr\bin",
  "$env:USERPROFILE\AppData\Local\nvim",
	"$env:USERPROFILE\AppData\Roaming\dystroy\broot\config",
	"$env:USERPROFILE\AppData\Roaming\helix",  
	"$env:USERPROFILE\AppData\Roaming\nushell",
  "$env:USERPROFILE\AppData\Roaming\topgrade",  
  "$env:USERPROFILE\.config\emacs",
	"$env:USERPROFILE\.config\wezterm",
	"$env:USERPROFILE\.config\todotxt-cli",
  "$env:USERPROFILE\.config\fish"
)

# Array of dotfiles to synchronize with tuples of source and destination
$dotfiles_to_be_synchronized = @(
		("$env:USERPROFILE\Code\dotfiles\.config\emacs", "$env:USERPROFILE\.config"),
    ("$env:USERPROFILE\Code\dotfiles\.config\wezterm", "$env:USERPROFILE\.config"),
		("$env:USERPROFILE\Code\dotfiles\.config\nvim", "$env:USERPROFILE\AppData\Local"),
		("$env:USERPROFILE\Code\dotfiles\.config\starship.toml", "$env:USERPROFILE\.config"),
		("$env:USERPROFILE\Code\dotfiles\.config\topgrade", "$env:USERPROFILE\AppData\Roaming"),
    ("$env:USERPROFILE\Code\dotfiles\.config\nushell", "$env:USERPROFILE\AppData\Roaming"),
    ("$env:USERPROFILE\Code\dotfiles\.config\broot\*", "$env:USERPROFILE\AppData\Roaming\dystroy\broot\config"),
    ("$env:USERPROFILE\Code\dotfiles\.config\helix", "$env:USERPROFILE\AppData\Roaming"),
		("$env:USERPROFILE\Code\dotfiles\usr\bin-windows\*", "$env:USERPROFILE\usr\bin"),
		("$env:USERPROFILE\Code\dotfiles\.gitconfig", "$env:USERPROFILE"),
    ("$env:USERPROFILE\Code\dotfiles\.config\todotxt-cli", "$env:USERPROFILE\.config"),
		("$env:USERPROFILE\Code\dotfiles\.config\fish", "$env:USERPROFILE\.config")
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
  } else {
			Write-Host "= Could not find $dotfile : skipping"
  }
}

# Create directories if they do not exist already
Write-HostWithTimestamp "Creating dotfiles directories if they do not exist"
foreach ($directory in $dotfiles_directories) {
  if (Test-Path $directory) {
    Write-Host "= Found $directory"
  } else {
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
