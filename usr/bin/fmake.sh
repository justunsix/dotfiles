#!/bin/bash

# Run make targets in the current directory and search
# for them using fzf

# Check if fzf is installed
if ! command -v fzf &>/dev/null; then
  echo "fzf is not installed. Please install it to use this script."
  exit 1
fi

# Check if a Makefile exists in the current directory
if [ ! -f Makefile ]; then
  echo "No Makefile found in the current directory."
  exit 1
fi

# Get the list of runnable targets from the Makefile, removing colons
targets=$(awk '/^[a-zA-Z0-9][^:]*:.*##/{gsub(/:$/, "", $1); print $1}' Makefile)

# Use fzf to select a target
selected_target=$(echo "$targets" | fzf --height 40% --reverse --inline-info --prompt "Select a target: ")

# Check if a target was selected and run make on it
if [ -n "$selected_target" ]; then
  echo "Executing make $selected_target..."
  make "$selected_target"
else
  echo "No target selected."
fi
