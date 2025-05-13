#!/usr/bin/env bash

source "$(dirname "$0")/common.sh"

# Clean Python .venv directories
clean_venvs() {

  # Change to the specified directory
  cd "$HOME/Code" || exit

  # Find and list all 'venv' directories
  local venv_dirs
  venv_dirs=$(find . -name ".venv" -type d -prune)

  if [ -z "$venv_dirs" ]; then
    echo "No '.venv' directories found."
    return
  fi

  # Display the size of each directory
  echo "The following '.venv' directories will be cleaned:"
  echo "$venv_dirs" | xargs du -chs

  # Prompt for confirmation
  read -p "Do you want to proceed with cleaning these directories? (y/n): " choice
  if [[ "$choice" != [Yy] ]]; then
    echo "Operation canceled."
    return
  fi

  # Remove the 'venv' directories
  echo "$venv_dirs" | xargs rm -rf
  echo "All specified 'venv' directories have been removed."
}

if command -v apt >/dev/null; then

  write_host_with_timestamp "Clear apt cache"
  apt clean

fi

if command -v pacman >/dev/null; then

  write_host_with_timestamp "Clear pacman cache"
  pacman -Sc --noconfirm

fi

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

if [ -d "$HOME/Code" ]; then

  write_host_with_timestamp "Clean python virtual environments"
  clean_venvs

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

# Clean carapace cache
## Cache contains completers, only needs clear on carapace configuration changes
if command -v carapace &>/dev/null; then
  write_host_with_timestamp "Clean carapace cache"
  carapace --clear-cache
fi

# Clean uv cache
if command -v uv &>/dev/null; then
  write_host_with_timestamp "Clean uv cache"
  uv cache clean
fi

# Clean Nix packages
if [ -e "$HOME/.nix-profile/" ] || [ -e "/nix/var/nix/profiles/" ]; then
  write_host_with_timestamp "Cleaning Nix packages"
  # Run nix package manager garbage collection
  # delete generations older than 30 days
  nix-collect-garbage --delete-older-than 30d
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
