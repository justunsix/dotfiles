function Write-HostWithTimestamp {
    param (
        [Parameter(Mandatory=$true)]
        [string]$Message
    )

$timestamp = Get-Date -Format "HH:mm"
Write-Host ("`n── " + $timestamp + " - " + $Message + " ─────────────")
}