#!/usr/bin/env bash

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
else
    # In Ubuntu
    echo ' '
    echo '----------------------------------------'
    echo 'Updating APT packages'
    echo ' '
    sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y
    sudo aptitude safe-upgrade -y

    # Update all Snaps
    echo ' '
    echo '----------------------------------------'
    echo 'Updating Snaps'
    echo ' '
    sudo snap refresh

    # update all deb-gets
    echo ' '
    echo '----------------------------------------'
    echo 'Updating deb-gets'
    echo ' '
    deb-get update && deb-get upgrade
fi

# Update all Flatpaks
echo ' '
echo '----------------------------------------'
echo 'Updating Flatpaks'
echo ' '
sudo flatpak update -y

# update main pip installed packages
# avoid upgrading all pip packages due to possible dependency issues
echo ' '
echo '----------------------------------------'
echo 'Updating pip packages'
echo ' '
python3 -m pip install --upgrade pip
python3 -m pip install --upgrade ansible
python3 -m pip install --upgrade tldr

# Update all Nix packages
echo ' '
echo '----------------------------------------'
echo 'Updating Nix packages'
echo ' '
nix-channel --update
nix-env -u

# Run nix package manager garbage collection
# If day is Friday, Saturday or Sunday ($(date +%u) -ge 5)
# If date is 1st of month or 20th or month
if [ $(date +%d) = 01 ] || [ $(date +%d) = 20 ]; then
    nix-collect-garbage -d
fi

# Scan font directories to build font cache files
# in case nix managed fonts were updated
echo ' '
echo '----------------------------------------'
echo 'Updating font cache'
echo ' '
fc-cache -v

# Update Clam, Freshclam sigantures
# if ClamAV is installed
if [ -f /usr/bin/clamscan ]; then
	echo ' '
	echo '----------------------------------------'
	echo 'Updating ClamAV'
	echo ' '
	sudo freshclam
fi
