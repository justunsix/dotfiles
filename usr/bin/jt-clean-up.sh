#!/usr/bin/env bash

source "$(dirname "$0")/common.sh"

# Clean up extra personal files such as:
# - Old nvm nodejs version

# nvm is a bash function, not a builtin, file or alias
if [ -d "$HOME/.nvm" ] && [ -s "$HOME/.nvm/nvm.sh" ]; then
		write_host_with_timestamp "Clean unused nvm versions"
		NVM_DIR="$HOME/.nvm"
		source "$NVM_DIR/nvm.sh"
		cd ~/.nvm/versions/node; ls -A | grep -v `nvm current` | xargs rm -rf
fi

# python venvs
if [ -d "$HOME/Code" ]; then

		write_host_with_timestamp "Clean python virtual environments"
		cd "$HOME/Code" || exit
		find . -name "venv" -type d -prune | xargs du -chs
		find . -name "venv" -type d -prune -exec rm -rf '{}' +

		# kondo command exists
		if command -v kondo >/dev/null; then
				write_host_with_timestamp "Clean software projects unneeded files"
				cd "$HOME/Code" && kondo
		fi

fi

# Clean Docker images
if command -v docker >/dev/null; then
		# check Docker daemon is running
		if docker info >/dev/null 2>&1; then
				write_host_with_timestamp "Clean Docker images"
				docker system prune -a
		else
   			echo 'Docker daemon is not running'
		fi
fi

# Clean screenshots
if [ -d "$HOME/Pictures/Screenshots" ]; then
		write_host_with_timestamp "Clean screenshots"
		rm -rf ~/Pictures/Screenshots/*
fi

# Clean mpv watch information
if [ -d "$HOME/.config/mpv/watch_later" ]; then
		write_host_with_timestamp "Clean mpv watch information"
		rm -rf ~/.config/mpv/watch_later/*
fi
