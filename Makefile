SHELL := bash

.PHONY: help
help: ## Show this help
	@egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.ONESHELL:
stow-files: ## Stow files
	# Link all files explicitly with --no-folding instead of symlink directories
	stow --target="$$HOME"/.config .config \
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
	stow --target="$$HOME" home --no-folding

unstow-files: ## Unstow files
	stow --target="$$HOME"/.config --delete .config --no-folding
	stow --target="$$HOME" --delete home --no-folding
	
