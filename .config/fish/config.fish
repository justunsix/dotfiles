if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Add ~/.local/bin to the path
set -gx PATH $PATH ~/.local/bin

# ---------------------------------------------------
# ---------------------------------------------------
# * My Customizations ---

# Set prompt to informative_vcs
set -g theme_display_vcs yes

# ---------------------------------------------------
# ** Completions

# Kubernetes Control fish autocompletion
kubectl completion fish | source

# pip autocompletion
pip completion --fish | source

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
# Ubuntu Debian per /usr/share/doc/fzf/README.Debian
echo fzf_key_bindings > ~/.config/fish/functions/fish_user_key_bindings.fish

# *** oc - Openshift CLI
# if oc command exists, source completions
if test -e ~/usr/bin/oc
		oc completion fish | source
end

# *** Starship
starship init fish | source
