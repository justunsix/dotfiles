#!/usr/bin/env bash

# Update git repositories using topgrade
# if topgrade is installed
if command -v topgrade >/dev/null; then
		echo ' '
		echo '----------------------------------------'
		echo 'Updating git repositories'
		echo ' '
		topgrade -y --only git_repos
fi

# Update all packages on the system

# Check if Linux Distribution is Fedora
if [ -f /etc/fedora-release ]; then

		echo ' '
		echo '----------------------------------------'
		echo 'Updating DNF packages'
		echo ' '
		sudo dnf check-update
		sudo dnf upgrade -y
		sudo dnf autoremove
fi

# If Linux distribution is Arch
if [ -f /etc/arch-release ]; then
		echo ' '
		echo '----------------------------------------'
		echo 'Updating pacman packages'
		echo ' '
		sudo pacman -Syu --noconfirm
fi

# If Linux distribution is Ubuntu, set isUbuntu variable to "true"
if [ -f /etc/os-release ]; then
    # Get os-release variables
    . /etc/os-release
    if [ "$ID" = "ubuntu" ]; then

				# In Ubuntu
				if command -v apt >/dev/null; then
						echo ' '
						echo '----------------------------------------'
						echo 'Updating APT packages'
						echo ' '
						sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y
						sudo aptitude safe-upgrade -y
				fi

				# Update all Snaps
				if command -v snap >/dev/null; then
						echo ' '
						echo '----------------------------------------'
						echo 'Updating Snaps'
						echo ' '
						sudo snap refresh
				fi

    fi
fi


if [ -f "$HOME/.config/home-manager/flake.nix" ]; then
		echo ' '
		echo '----------------------------------------'
		echo 'Updating Nix Flake and Home Manager'
		echo ' '
		cd "$HOME/.config/home-manager/" || exit
    nix flake update && home-manager switch -b bak
		cd - || exit
fi

# Update all Flatpaks
if command -v flatpak >/dev/null; then
		echo ' '
		echo '----------------------------------------'
		echo 'Updating Flatpaks'
		echo ' '
		sudo flatpak update -y
fi

# update main pip installed packages
# avoid upgrading all pip packages due to possible dependency issues
# unless pipx is installed
if command -v python3 >/dev/null; then
		if command -v pipx >/dev/null; then
				echo ' '
				echo '----------------------------------------'
				echo 'Updating pipx packages'
				echo ' '
				pipx upgrade-all
		else
				if command -v pip >/dev/null; then
						echo ' '
						echo '----------------------------------------'
						echo 'Updating pipx packages'
						echo ' '
						python3 -m pip install --user --upgrade pip
						python3 -m pip install --user --upgrade ansible
						python3 -m pip install --user --upgrade tldr
				fi
		fi
fi

# Update all Nix packages
if [ -e "$HOME/.nix-profile/" ]; then
		echo ' '
		echo '----------------------------------------'
		echo 'Updating Nix packages'
		echo ' '
		nix-channel --update
		nix-env -u
fi

# If day is Saturday or Sunday
# or if first argument was true
if [ "$(date +%u)" -ge 5 ] || [ "$1" = "true" ]; then

    # Update Emacs Packages
		if command -v emacs >/dev/null; then
				echo ' '
				echo '----------------------------------------'
				echo 'Updating Emacs packages'
				echo ' '
				# emacs --batch --eval '(progn (package-refresh-contents) (package-upgrade-all))'
				emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(auto-package-update-now)'
				emacs --batch -l ~/.config/emacs/setup/jt-emacs-package-managers.el --eval '(straight-pull-all)'
		fi

		# update all deb-gets
		if command -v deb-get >/dev/null; then
				echo ' '
				echo '----------------------------------------'
				echo 'Updating deb-gets'
				echo ' '
				deb-get update && deb-get upgrade
		fi

		# Scan font directories to build font cache files
		# in case nix managed fonts were updated
		echo ' '
		echo '----------------------------------------'
		echo 'Updating font cache'
		echo ' '
		fc-cache -v

		# nvm is a bash function, not a builtin, file or alias
		# from https://github.com/branneman/dotfiles/blob/master/scripts/updates
		if [ -d "$HOME/.nvm" ] && [ -s "$HOME/.nvm/nvm.sh" ]; then
				echo ' '
				echo '----------------------------------------'
				echo 'Update nodejs version in nvm'
				echo ' '
				NVM_DIR="$HOME/.nvm"
				source "$NVM_DIR/nvm.sh"
				nvm install lts/*
				nvm alias default lts/*
				nvm use default
		fi

		# Update Clam, Freshclam sigantures
		# if ClamAV is installed
		if [ -f /usr/bin/clamscan ]; then
				echo ' '
				echo '----------------------------------------'
				echo 'Updating ClamAV'
				echo ' '
				sudo freshclam
		fi

		# Update conda and its packages in base environment
		# if conda is installed
		if command -v conda >/dev/null; then
				echo ' '
				echo '----------------------------------------'
				echo 'Updating conda'
				echo ' '
				conda update -n base conda -c anaconda --yes
				conda update --all --yes
				# clean unused packages
				conda clean --all --yes
		fi

		# Clean Nix packages
		if [ -e "$HOME/.nix-profile/" ]; then
				echo ' '
				echo '----------------------------------------'
				echo 'Cleaning Nix packages'
				echo ' '
				# Run nix package manager garbage collection
				# delete generations older than 30 days
				nix-collect-garbage --delete-older-than 30d
		fi

fi
