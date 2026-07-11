SHELL := bash

.PHONY: help
help: ## Show this help
	@egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.ONESHELL:
stow-files: ## Stow files
	# Link all files explicitly with --no-folding instead of symlink directories
	git clone https://github.com/LazyVim/starter ~/.config/nvim-lazyvim
	rm ~/.config/nvim-lazyvim/init.lua
	rm ~/.config/nvim-lazyvim/lua/config/autocmds.lua
	rm ~/.config/nvim-lazyvim/lua/config/keymaps.lua
	rm ~/.config/nvim-lazyvim/lua/config/lazy.lua
	rm ~/.config/nvim-lazyvim/lua/config/options.lua
	stow --target=/home/justin/.config .config \
	--no-folding \
	--ignore=emacs \
	--ignore=fish \
	--ignore=home-manager \
	--ignore=i3 \
	--ignore=i3status \
	--ignore=input-remapper \
	--ignore=navi \
	--ignore=rofi \
	--ignore=tmux	

unstow-files: ## Unstow files
	stow --target=/home/justin/.config --delete .config --no-folding
	
