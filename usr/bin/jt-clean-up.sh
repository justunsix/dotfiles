#!/usr/bin/env bash

# Clean up extra personal files such as:
# - Old nvm nodejs version

# nvm is a bash function, not a builtin, file or alias
if [ -d "$HOME/.nvm" ] && [ -s "$HOME/.nvm/nvm.sh" ]; then
		echo 'Clean unused nvm versions'
		NVM_DIR="$HOME/.nvm"
		source "$NVM_DIR/nvm.sh"
		cd ~/.nvm/versions/node || exit; ls -A | grep -v 'nvm current' | xargs rm -rf
fi

# python venvs
if [ -d "$HOME/Code" ]; then

		echo 'Clean python venvs folders from projects, displaying impacted folders if any:'
		cd "$HOME/Code" || exit
		find . -name "venv" -type d -prune | xargs du -chs
		find . -name "venv" -type d -prune -exec rm -rf '{}' +

		# kondo command exists
		if command -v kondo >/dev/null; then
				echo 'Clean software projects unneeded files'
				cd "$HOME/Code" && kondo
		fi

fi

# Clean Docker images
if command -v docker >/dev/null; then
		# check Docker daemon is running
		if docker info >/dev/null 2>&1; then
				echo 'Clean Docker images'
				docker system prune -a
		else
   			echo 'Docker daemon is not running'
		fi
fi

# Clean mpv watch information
if [ -d "$HOME/.config/mpv/watch_later" ]; then
		echo 'Clean mpv watch information'
		rm -rf ~/.config/mpv/watch_later/*
fi
