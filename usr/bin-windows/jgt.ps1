# Check folders at folder ~/Code and run git status in each folder
# to find which folders have changes
# Check if folder is a git repository, if so, run git status, otherwise skip
# the folder
# Run git status and check output,
# if output contains "nothing to commit, working tree clean", then echo "."
# else print the working directory

# Get list of folders in ~/Code
$folders = Get-ChildItem -Path ~/Code -Directory

function Check-GitStatus {
		param (
				$folders
		)
		foreach ($folder in $folders) {
				# Check if folder is a git repository
				if (Test-Path -Path "$($folder.FullName)\.git") {
						# Run git status
						$git_status = git -C $folder.FullName status
						# Check git status output
						if ($git_status -match "nothing to commit, working tree clean") {
								# If output contains "nothing to commit, working tree clean"
								# then echo "."
								Write-Host "." -NoNewline
						} else {
								# If output does not contain "nothing to commit, working tree clean"
								# then print the working directory
								Write-Host "`n"$folder.FullName "has changes" -ForegroundColor Green
								git status
						}
				}
		}
}

Check-GitStatus($folders)
