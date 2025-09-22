SHELL := /bin/bash

CONFIG_FOLDER_WINDOWS := ~/AppData/Local/nvim-aon/
CONFIG_FOLDER_NIX := ~/.config/nvim-aon/

.PHONY: help
help: ## Show this help
	@egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.ONESHELL:
setup: ## Setup folders if they do not already exist
	if [ -d /c/Users ]; then
		rm -rf $$CONFIG_FOLDER_WINDOWS
		echo "On Windows, creating config folder"
		mkdir -p ${CONFIG_FOLDER_WINDOWS} 
	else
		rm -rf $$CONFIG_FOLDER_NIX
		echo "On Unix/Linux, creating config folder"
		mkdir -p ${CONFIG_FOLDER_NIX} 
	fi

.ONESHELL:
install: setup ## Copy Neovim example configuration to configuration folders
	folder=${CONFIG_FOLDER_NIX}
	if [ -d /c/Users ]; then
		folder=${CONFIG_FOLDER_WINDOWS}
		cp -r ./* $$folder
	else
		# Use stow
		if command -v stow &>/dev/null; then
			cd ..
			stow -t $$folder nvimexample
		else
			echo "Error: Install stow to symlink Neovim configuration files"
		fi
	fi
	cd $$folder
	printf "\nCopied/Symlinked data to:\n"
	pwd
	ls -al
