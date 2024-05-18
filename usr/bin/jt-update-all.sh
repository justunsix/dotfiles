#!/usr/bin/env bash

source "$(dirname "$0")/common.sh"

# Update all packages on the system

# Fedora packages
if command -v dnf >/dev/null; then
	write_host_with_timestamp "Updating DNF packages"
	sudo dnf check-update
	sudo dnf upgrade -y
	sudo dnf autoremove
fi

# Arch packages
if command -v pacman >/dev/null; then
	write_host_with_timestamp "Updating pacman packages"
	sudo pacman -Syu --noconfirm
fi

# Debian, Ubuntu apt packages
if command -v apt >/dev/null; then
	write_host_with_timestamp "Updating APT packages"
	sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y
	sudo aptitude safe-upgrade -y
fi

# Snap packages
if command -v snap >/dev/null; then
	write_host_with_timestamp "Updating Snaps"
	sudo snap refresh
fi

# Homebrew packages
if command -v brew >/dev/null; then
	write_host_with_timestamp "Updating Homebrew packages"
	brew update && brew upgrade && brew cleanup
fi

# Update git repositories using topgrade
# if topgrade is installed
if command -v topgrade >/dev/null; then
	write_host_with_timestamp "Updating git repositories"
	topgrade -y --only git_repos
fi

if [ -f "$HOME/.config/home-manager/flake.nix" ]; then
	write_host_with_timestamp "Updating Nix Flake and Home Manager"
	cd "$HOME/.config/home-manager/" || exit
	nix flake update && home-manager switch -b bak
	cd - || exit
fi

# Update all Flatpaks
if command -v flatpak >/dev/null; then
	write_host_with_timestamp "Updating Flatpaks"
	sudo flatpak update -y
fi

# update main pip installed packages
# avoid upgrading all pip packages due to possible dependency issues
# unless pipx is installed
if command -v python3 >/dev/null; then
	if command -v pipx >/dev/null; then
		write_host_with_timestamp "Updating pipx packages"
		pipx upgrade-all
	else
		if command -v pip >/dev/null; then
			write_host_with_timestamp "Updating pipx packages"
			python3 -m pip install --user --upgrade pip
			python3 -m pip install --user --upgrade ansible
			python3 -m pip install --user --upgrade tldr
		fi
	fi
fi

# Update all Nix packages
if [ -e "$HOME/.nix-profile/" ] || [ -e "/nix/var/nix/profiles/" ]; then
	write_host_with_timestamp "Updating Nix packages"
	nix-channel --update
	nix-env -u
	if [ -x "$(command -v home-manager)" ]; then
		home-manager switch
	fi
fi

# If day is Saturday or Sunday
# or if first argument was true
if [ "$(date +%u)" -ge 5 ] || [ "$1" = "true" ]; then

	# Update Emacs Packages
	if command -v emacs >/dev/null; then
		if [ -f ~/.config/emacs/setup/jt-emacs-package-managers.el ]; then
			write_host_with_timestamp "Updating Emacs packages"
			emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(auto-package-update-now)'
			emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(straight-pull-all)'
		fi
	fi

	# update all deb-gets
	if command -v deb-get >/dev/null; then
		write_host_with_timestamp "Updating deb-gets"
		deb-get update && deb-get upgrade
	fi

	# Scan font directories to build font cache files
	# in case nix managed fonts were updated
	write_host_with_timestamp "Updating font cache"
	fc-cache -v

	# nvm is a bash function, not a builtin, file or alias
	# from https://github.com/branneman/dotfiles/blob/master/scripts/updates
	if [ -d "$HOME/.nvm" ] && [ -s "$HOME/.nvm/nvm.sh" ]; then
		write_host_with_timestamp "Update nodejs version in nvm"
		NVM_DIR="$HOME/.nvm"
		source "$NVM_DIR/nvm.sh"
		nvm install lts/*
		nvm alias default lts/*
		nvm use default
	fi

	# Update Clam, Freshclam sigantures
	# if ClamAV is installed
	if [ -f /usr/bin/clamscan ]; then
		write_host_with_timestamp "Updating ClamAV"
		sudo freshclam
	fi

	# Update conda and its packages in base environment
	# if conda is installed
	if command -v conda >/dev/null; then
		write_host_with_timestamp "Updating conda"
		conda update -n base conda -c anaconda --yes
		conda update --all --yes
		# clean unused packages
		conda clean --all --yes
	fi

	# Clean Nix packages
	if [ -e "$HOME/.nix-profile/" ] || [ -e "/nix/var/nix/profiles/" ]; then
		write_host_with_timestamp "Cleaning Nix packages"
		# Run nix package manager garbage collection
		# delete generations older than 30 days
		nix-collect-garbage --delete-older-than 30d
	fi

fi
