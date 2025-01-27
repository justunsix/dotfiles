# Script uses template from https://gist.github.com/deadlydog/d04b5d43170a90d8bc0143373d90010f
<#
	.SYNOPSIS
	Update software packages and their related information

    .DESCRIPTION
    Uses topgrade to update software and a clean up script to remove old and temporary files

    .PARAMETER acceptAll
    Accept all prompts

    .EXAMPLE
    .\jt-update-all.ps1 -acceptAll "false"

#>
[CmdletBinding()]
param (
	[Parameter(Mandatory = $false, HelpMessage = 'Accept all prompts.')]
	[string] $acceptAll = 'false'
)

process {
	# Load the common functions and variables
	. $PSScriptRoot/common.ps1

	# Update with topgrade
	# disable system updates on Windows for now
	# Prompt user whether to run topgrade
	Write-HostWithTimestamp "Topgrade"
	if ($acceptAll -eq "true") {
		$runTopgrade = "y"
	}
	else {
		$runTopgrade = Read-Host "Do you want to run topgrade? (y/n)"
	}

	# If user wants to run topgrade, then run topgrade with options
	if ($runTopgrade -eq "y") {
		if ($acceptAll -eq "true") {
			$runTopgradeWithPS1 = "y"
		}
		else {
			$runTopgradeWithPS1 = Read-Host "Do you want to run topgrade with powershell, emacs? (y/n)"
		}

		Write-Host "`nRunning topgrade" -ForegroundColor Green

		# Disable vagrant due to large updates, winget due to prompts, update neovim manually
		if ($runTopgradeWithPS1 -eq "y") {
      Write-Host "`nwith --disable system vagrant winget vim"
			topgrade -y --disable system vagrant winget vim
		}
		else {
			Write-Host "`nwith --disable powershell vagrant emacs vim winget" -ForegroundColor Green
			topgrade -y --disable system --disable powershell vagrant emacs vim winget
		}
	}

	# Check if emacs command exists, if so update packages
	if (Test-Path ~\scoop\apps\emacs\current\bin\emacs.exe) {
		if (Test-Path ~/.config/emacs/setup/jt-emacs-package-managers.el) {
			Write-HostWithTimestamp "Updating Emacs packages"
			emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(auto-package-update-now)'
			emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(straight-pull-all)'
		}
	}

	jt-clean-up.ps1

}

begin {

	# Put Functions here

	$InformationPreference = 'Continue'
	$VerbosePreference = 'Continue' # Uncomment this line if you want to see verbose messages.

	# Log all script output to a file for easy reference later if needed.
	# [string] $lastRunLogFilePath = "$PSCommandPath.LastRun.log"
	# Start-Transcript -Path $lastRunLogFilePath

	# Display the time that this script started running.
	[DateTime] $startTime = Get-Date
	Write-Information "Starting script at '$($startTime.ToString('u'))'."
}

end {
	# Display the time that this script finished running, and how long it took to run.
	[DateTime] $finishTime = Get-Date
	[TimeSpan] $elapsedTime = $finishTime - $startTime
	Write-Information "Finished script at '$($finishTime.ToString('u'))'. Took '$elapsedTime' to run."

	# Stop-Transcript
}
