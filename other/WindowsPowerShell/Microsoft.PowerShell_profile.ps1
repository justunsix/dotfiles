# Windows PowerShell 5.1 PROFILE
# Edit like notepad $PROFILE
# Stored in $PROFILE variable
# Managed by Ansible

# Chocolately
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}

# Additional PowerShell capabitilities if needed
function Initialize-Profile {

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

		# if Broot present, source function
		if (Test-Path $env:USERPROFILE\AppData\Roaming\dystroy\broot\config\launcher\powershell\br.ps1) {
				. $env:USERPROFILE\AppData\Roaming\dystroy\broot\config\launcher\powershell\br.ps1
		}

}

# Simplify powershell.exe to be close to -noprofile
$loadAll = $false

if ($loadAll) {

	Initialize-Profile
	
}

Write-Host "Load full profile with Initialize-Profile"
