#!/usr/bin/env bash

# For use in a folder with a number git repositories in sub-folders
# within this folder.
# This script will loop through each child folder and execute a git status in each one.

cd ~/Code/

# Loop through each folder in the current directory
for dir in */; do
		# Change directory into the folder
		cd $dir

		# If not a git repository, then skip
		if ! git rev-parse --git-dir >/dev/null 2>&1; then
				# Not a git repository
				cd ..
		else
				# Run git status and check output, if output is nothing to commit, working tree clean
				# Then echo "." without newline
				# Else print the working directory
				if git status | grep -q "nothing to commit, working tree clean" || git status | grep -q "fatal: not a git repository (or any parent up to mount point /)" || git status | grep -q "Stopping at filesystem boundary (GIT_DISCOVERY_ACROSS_FILESYSTEM not set)."; then
						echo -n "."

				else
						echo "Working directory has changes: ~/Code/$dir"
				fi
				# Change directory back to the parent folder
				cd ..
		fi
done

# Exit the script
exit 0
