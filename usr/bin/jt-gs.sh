#!/usr/bin/env bash

# For use in a folder with a number git repositories in sub-folders
# within this folder.
# This script will loop through each child folder and execute a git pull in each one.

# For each child directory in this folder
# 1. Change directory into the folder
# 2. Execute git pull
# 3. Change directory back to the parent folder
# 4. Repeat for each child folder
# 5. Exit

# Loop through each folder in the current directory
for dir in */; do
  # Change directory into the folder
  cd "$dir" || { echo "Error: accessing directory"; exit 1; }

  # If not a git repository, then skip
  if ! git rev-parse --git-dir >/dev/null 2>&1; then
    # Not a git repository
    cd ..
  else
    # Echo the current directory
    echo ' '
    echo '----------------------------------------'
    echo 'Repo: ' "$dir"
    # Execute git pull
    git pull
    # Change directory back to the parent folder
    cd ..
  fi
done

# Exit the script
exit 0
