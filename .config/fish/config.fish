if status is-interactive
    # Commands to run in interactive sessions can go here
		if type -q atuin
			atuin init fish --disable-ctrl-r | source
		end
end

# Add ~/.local/bin and ~/usr/bin to the path
set -gx PATH $PATH ~/.local/bin ~/usr/bin

# ---------------------------------------------------
# ---------------------------------------------------
# * My Customizations ---

# Set prompt to informative_vcs
set -g theme_display_vcs yes

# ---------------------------------------------------
# ** Completions

# If kubectl command is present, source completion
if type -q kubectl
		kubectl completion fish | source
end

if type -q pip
	# pip autocompletion
	pip completion --fish | source
end

# *** oc - Openshift CLI
# if oc command exists, source completions
if type -q oc
		oc completion fish | source
end

if type -q carapace
	# https://carapace-sh.github.io/carapace-bin/setup.html
	set -Ux CARAPACE_BRIDGES 'zsh,fish,bash,inshellisense' # optional
	mkdir -p ~/.config/fish/completions
	carapace --list | awk '{print $1}' | xargs -I{} touch ~/.config/fish/completions/{}.fish # disable auto-loaded completions (#185)
	carapace _carapace | source
end

# ---------------------------------------------------
# ** Aliases

# Source .bash_aliases
if test -e $HOME/.bash_aliases
		source $HOME/.bash_aliases
end

# Source overlay variables, aliases
if test -e $HOME/.env-aliases
		source $HOME/.env-aliases
end

# Check if WezTerm Flatpak is installed
if type -q flatpak
  flatpak list | grep -iq 'org.wezfurlong.wezterm' > /dev/null
  if test $status -eq 0
  		alias wezterm="flatpak run org.wezfurlong.wezterm"
  end
end  

alias nvm="echo 'Switch to bash shell to use nvm'"

# ---------------------------------------------------
# ** Programs
## cat(1) clone with syntax highlighting and git integration
# alias bat="batcat"

# *** fzf
# On Fedora fzf
if test -e /usr/share/fzf/shell/key-bindings.fish
		source /usr/share/fzf/shell/key-bindings.fish
end

if type -q fzf_key_bindings
	 # Ubuntu Debian, Arch per /usr/share/doc/fzf/README.Debian
	 echo fzf_key_bindings > ~/.config/fish/functions/fish_user_key_bindings.fish
end

if type -q starship
	 # *** Starship
	 starship init fish | source
end

if type -q zoxide
	 # zoxide - smarter cd
	 zoxide init fish | source
end

## conda - package manager

### Only load conda if called
### https://stackoverflow.com/questions/52706888/anaconda-python-causing-slow-terminal-startup-prompt/73910386#73910386
function conda-init -d "initialize conda shell functions"
		# Initialize conda if it is an alias
		# else set up conda with fish config / path
    if type conda | grep -q alias
        echo "initializing conda..."
				eval $HOME/miniconda3/bin/conda "shell.fish" "hook" $argv | source
    else
			if test -f "$HOME/miniconda3/etc/fish/conf.d/conda.fish"
					. "$HOME/miniconda3/etc/fish/conf.d/conda.fish"
			else
					set -x PATH "$HOME/miniconda3/bin" $PATH
			end
	end
end

alias conda "conda-init; conda"

# if type -q conda 
# 	# >>> conda initialize >>>
# 	# !! Contents within this block are managed by 'conda init' !!
# 	if test -f $HOME/miniconda3/bin/conda
# 			eval $HOME/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# 	else
# 			if test -f "$HOME/miniconda3/etc/fish/conf.d/conda.fish"
# 					. "$HOME/miniconda3/etc/fish/conf.d/conda.fish"
# 			else
# 					set -x PATH "$HOME/miniconda3/bin" $PATH
# 			end
# 	end
# 	# <<< conda initialize <<<
# end	

## Managed by nix's conda-shell
# if type -q conda-shell
# 	# >>> conda initialize >>>
# 	# !! Contents within this block are managed by 'conda init' !!
# 	eval $HOME/.conda/bin/conda "shell.fish" "hook" $argv | source
# 	# <<< conda initialize <<<
# 	# deactivate base environment until called
# 	conda deactivate
# end

## Yazi File Manager
## Wrapper for yazi to allow change cwd when existing yazi
function y
    set tmp (mktemp -t "yazi-cwd.XXXXXX")
    yazi $argv --cwd-file="$tmp"
    if set cwd (command cat -- "$tmp"); and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
        builtin cd -- "$cwd"
    end
    rm -f -- "$tmp"
end
