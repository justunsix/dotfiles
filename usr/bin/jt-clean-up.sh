#!/usr/bin/env bash

# Clean up extra personal files such as:
# - Old nvm nodejs version

# nvm is a bash function, not a builtin, file or alias
if [ -d "$HOME/.nvm" ] && [ -s "$HOME/.nvm/nvm.sh" ]; then
		echo 'Clean unused nvm versions'
		NVM_DIR="$HOME/.nvm"
		source "$NVM_DIR/nvm.sh"
		cd ~/.nvm/versions/node; ls -A | grep -v `nvm current` | xargs rm -rf
fi

# node_modules
if [ -d "$HOME/Code" ]; then

		echo 'Clean node_modules folders from projects'
		cd ~/Code
		find . -name "node_modules" -type d -prune | xargs du -chs
		find . -name "node_modules" -type d -prune -exec rm -rf '{}' +
fi

# python venvs
if [ -d "$HOME/Code" ]; then

		echo 'Clean python venvs folders from projects'
		cd ~/Code
		find . -name "venv" -type d -prune | xargs du -chs
		find . -name "venv" -type d -prune -exec rm -rf '{}' +
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
