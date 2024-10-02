# Install Tabby AI Coding Assistant
# for Windows with NVIDIA CUDA Support
# based on https://tabby.tabbyml.com/docs/quick-start/installation/windows/

# Tabby version to install
## With NVIDIA CUDA version 12.2 or higher support
$tabbyVersion = "*tabby_x86_64-windows-msvc-cuda122*"
# Define the extraction directory where Tabby will be installed
$extractionDir = "$env:USERPROFILE\usr\bin\tabbyml"

# Check if Tabby is already installed
if (Test-Path $extractionDir) {
  Write-Host "Tabby is already installed in $extractionDir."
  exit 0
}
else {
  # Create the extraction directory if it doesn't exist
  New-Item -ItemType Directory -Path $extractionDir | Out-Null   
}

# Define the GitHub repository and release URL
$repo = "TabbyML/tabby"
$releaseUrl = "https://api.github.com/repos/$repo/releases/latest"

# Get the latest release information
$releaseInfo = Invoke-RestMethod -Uri $releaseUrl

# Find the asset that matches the platform 
$asset = $releaseInfo.assets | Where-Object { $_.name -like $tabbyVersion }

if (-not $asset) {
  Write-Error "No suitable asset found for Windows."
  exit 1
}

# Define the download URL and destination file
$downloadUrl = $asset.browser_download_url
$destinationFile = "$env:TMP\tabby.zip"

# Download the latest release
Write-Host "Downloading Tabby from $downloadUrl..."
Invoke-WebRequest -Uri $downloadUrl -OutFile $destinationFile

# Create the extraction directory if it doesn't exist
if (-not (Test-Path $extractionDir)) {
  New-Item -ItemType Directory -Path $extractionDir | Out-Null
}

# Extract the contents of the ZIP file
Write-Host "Extracting Tabby to $extractionDir..."
Expand-Archive -Path $destinationFile -DestinationPath $extractionDir -Force

# Delete the downloaded ZIP file
Remove-Item -Path $destinationFile

Write-Host "Tabby has been successfully installed to: $extractionDir"

Write-Host "Checking Tabby prerequisite NVIDIA CUDA Toolkit for GPU..."

if (-not (Get-Command "nvcc" -ErrorAction SilentlyContinue)) {
  Write-Host "CUDA command 'nvcc' is not available. Please download appropriate version from https://developer.nvidia.com/cuda-toolkit"
}
else {
  Write-Host "CUDA is available."
}
