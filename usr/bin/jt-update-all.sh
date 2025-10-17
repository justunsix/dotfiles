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
    # Update Arch keyring first, only required for systems
    # that have not been updated in a while to prevent package signature trust issues
    sudo pacman -Sy archlinux-keyring --noconfirm
    sudo pacman -Syu --noconfirm
fi

# Debian, Ubuntu apt packages
if command -v apt >/dev/null; then
    write_host_with_timestamp "Updating APT packages"
    sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y && sudo apt clean -y
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
    fi
fi

# Update all Nix packages
if [ -f "$HOME/.config/home-manager/flake.nix" ]; then
    write_host_with_timestamp "Updating Nix Flake and Home Manager"
    cd "$HOME/.config/home-manager/" || exit
    # Update flakes and home-manager
    # -b backup : if home-manager finds conflicting files, make backup
    nix flake update && home-manager switch -b backup
    cd - || exit
fi

if [ -e "$HOME/.nix-profile/" ] || [ -e "/nix/var/nix/profiles/" ]; then
    write_host_with_timestamp "Updating Nix packages"
    nix-channel --update
    if [ -x "$(command -v home-manager)" ]; then
        home-manager switch -b backup
    fi

    # Check if memory is at least 8 GB before running nix-env update
    # if less than 8 GB, nix-env -u will be too slow and should not be run
    # Get the total RAM in GB
    total_ram=$(free -g | awk '/^Mem:/{print $2}')

    # Check if total RAM is at least 8GB
    if [ "$total_ram" -ge 8 ]; then
        echo "The system has at least 8GB of RAM. Running nix-env -u"
        nix-env -u
    else
        echo "The system has less than 8GB of RAM. Skipping nix-env updates"
    fi

fi

# Update yazi packages
if command -v yazi >/dev/null; then
    write_host_with_timestamp "Updating yazi packages"
    ya pkg upgrade
fi

# Update television (tv) channels
if command -v tv >/dev/null; then
    write_host_with_timestamp "Updating television channels"
    tv update-channels
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

fi
