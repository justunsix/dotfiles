# Install PowerShell Core modules to a custom directory

# Function
function Install-ModuleToDirectory {
    [CmdletBinding()]
    [OutputType('System.Management.Automation.PSModuleInfo')]
    param (
        [Parameter(Mandatory = $true)]
        [ValidateNotNullOrEmpty()]
        $Name,

        [Parameter(Mandatory = $true)]
        [ValidateScript({ Test-Path $_ })]
        [ValidateNotNullOrEmpty()]
        $Destination
    )

    # Check if the module is already installed
    if (-not (Test-Path (Join-Path $Destination $Name))) {
        # Install the module to the custom destination
        Find-Module -Name $Name -Repository 'PSGallery' | Save-Module -Path $Destination
    }

    # Import the module from the custom directory
    Import-Module -FullyQualifiedName (Join-Path $Destination $Name)

    # Return the module info
    return (Get-Module)
}

# Example usage:
# Install-ModuleToDirectory -Name 'YourModuleName' -Destination 'E:\Modules'

# --- Windows Powershell 5.1 modules

# Install Readline supports
Install-ModuleToDirectory -Name PSReadline -Destination "~\scoop\modules"
# Install-Module -Name PSReadline -AllowClobber -Force
Install-ModuleToDirectory -Name PSFzf -Destination "~\scoop\modules"
## Azure Active Directory, deprecated 2023-06
Install-ModuleToDirectory -Name AzureADPreview -Destination "~\scoop\modules"

# --- Powershell modules

Install-ModuleToDirectory -Name Az.Accounts  -Destination "~\scoop\modules"
Install-ModuleToDirectory -Name Az.Tools.Predictor -Destination "~\scoop\modules"
# Install-ModuleToDirectory -Name AzureAD -Destination "~\scoop\modules"
Install-ModuleToDirectory -Name CompletionPredictor -Destination "~\scoop\modules"
Install-ModuleToDirectory -Name Microsoft.Graph -Destination "~\scoop\modules"

# Install-ModuleToDirectory -Name SqlServer -Destination "~\scoop\modules"
# Install-ModuleToDirectory -Name Terraform -Destination "~\scoop\modules"