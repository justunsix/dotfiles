# Install DevPod CLI
# https://devpod.sh/docs/getting-started/install

# Make devpod directory
md -Force "$Env:APPDATA\devpod"
# Set security protocol
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]'Tls,Tls11,Tls12';

# Download latest devpod cli
Invoke-WebRequest -URI "https://github.com/loft-sh/devpod/releases/latest/download/devpod-windows-amd64.exe" -OutFile $Env:APPDATA\devpod\devpod.exe

# Include devpod in path
$env:Path += ";" + $Env:APPDATA + "\devpod";
[Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::User);

Write-Host "DevPod CLI Installed at $Env:APPDATA\devpod"

# Install Azure provider
# using CanadaCentral region
$env:AZURE_REGION = "CanadaCentral"
# DevPod should be able to pull other Azure environment information from the local environment
# or previous az cli work
devpod provider add azure
