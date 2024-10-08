#!/usr/bin/env bash

source "$(dirname "$0")/common.sh"

# Clean up extra personal files such as:
# - Old nvm nodejs version

# nvm is a bash function, not a builtin, file or alias
# remove older nvm versions
if [ -d "$HOME/.nvm" ] && [ -s "$HOME/.nvm/nvm.sh" ]; then
  write_host_with_timestamp "Clean unused nvm versions"
  NVM_DIR="$HOME/.nvm"
  source "$NVM_DIR/nvm.sh"
  cd ~/.nvm/versions/node
  ls -A | grep -v $(nvm current) | xargs rm -rf
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

if command -v yazi >/dev/null; then

  write_host_with_timestamp "Clear yazi cache"
  yazi --clear-cache

fi

# Clean screenshots
if [ -d "$HOME/Pictures/Screenshots" ]; then
  write_host_with_timestamp "Clean screenshots"
  rm -rf ~/Pictures/Screenshots/*
fi

# Clean mpv watch information
if [ -d "$HOME/.config/mpv/watch_later" ] || [ -d "$HOME/.local/state/mpv/watch_later" ]; then
  write_host_with_timestamp "Clean mpv watch information"
  rm -rf "$HOME"/.config/mpv/watch_later/*
  rm -rf "$HOME"/.local/state/mpv/watch_later/*
fi

# Clean Emacs and Doom Packages
if [ -d "$HOME/.config/emacs/bin" ]; then
  write_host_with_timestamp "Clean Doom Emacs Packages"
  cd "$HOME/.config/emacs/bin" && doom gc
fi
if [ -d "$HOME/.config/emacs/.local/cache" ]; then
  write_host_with_timestamp 'Clean non-essential Emacs cache'
  cd "$HOME/.config/emacs/.local/cache" || exit
  rm -rf autosave
  rm -rf org
  rm -rf undo-fu-session
  rm -f savehist

  if [ "$1" == "--all" ]; then
    write_host_with_timestamp "Clean Emacs cache"
    cd "$HOME/.config/emacs/.local/cache" && rm -rf *
  else
    echo 'To clean complete Emacs cache (warning it will remove saved project, recent and files history), run:'
    echo 'jt-clean-up.sh --all'
  fi
fi
