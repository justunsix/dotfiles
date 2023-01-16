# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# Added via Ansible

# If not running interactively, don't do anything
case $- in
*i*) ;;
*) return ;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
xterm-color | *-256color) color_prompt=yes ;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm* | rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*) ;;

esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# ---------------------------------------------------
# --- My Customizations ---

# Overlay to apply to dotfiles in this repository such as overrides and other environment variables
export DOTFILES_OVERLAY_DIR="$HOME/Code/dotfiles-overlay"

# Source overlay variables, aliases
if [ -f "$DOTFILES_OVERLAY_DIR/.env" ]; then
		source "$DOTFILES_OVERLAY_DIR/.env"
    # aliases
		source "$DOTFILES_OVERLAY_DIR/.env-aliases"
fi

isUbuntu="false"
isFedora="false"
isWSLUbuntu="false"

# If Linux distribution is Ubuntu, set isUbuntu variable to "true"
if [ -f /etc/os-release ]; then
    # Get os-release variables
    . /etc/os-release
    if [ "$ID" = "ubuntu" ]; then
        isUbuntu="true"
    fi
fi

# If file exist at /mnt/c, it is WSL
if [ -f /mnt/c/Windows/System32/wsl.exe ]; then
    isWSLUbuntu="true"
    isUbuntu="false"
fi

# If Linux distribution is Fedora, set isFedora variable to "true"
if [ -f /etc/fedora-release ]; then
    isFedora="true"
fi

# User specific environment variables and startup programs
# per https://wiki.archlinux.org/title/Environment_variables

export QT_QPA_PLATFORMTHEME="qt5ct"

export PATH="$HOME/usr/bin/phantomjs-2.1.1-linux-x86_64/bin:$HOME/.nix-profile/bin/:$HOME/usr/bin/todotxt-cli:$HOME/.local/bin:$HOME/usr/bin:$PATH"

export EMACS_SERVER_FILE="~/.emacs.d/server/server"
# Set EDITOR environment variable to emacs
export EDITOR="emacs"
# Set xmonad configuration director
# export XMONAD_CONFIG_DIR="$HOME/.config/xmonad"

# Standard programs
## cat(1) clone with syntax highlighting and git integration
# alias bat="batcat"

# Emacs
# Ensure in emacs (start-server) is done in init or on emacs command line
# Find emacs server socks using:
# lsof -c emacs | grep server | tr -s " "
# Default on Linux is "/run/user/1000/emacs/server"
# alias emacsclient="emacsclient --socket-name="/run/user/1000/emacs/server"

# Node Version Manager (NVM)
export NVM_DIR="$HOME/.nvm"
# This loads nvm
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
# This loads nvm bash_completion
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# Haskell - Glasgow Haskell Compiler
# ghcup-env
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" 

# fzf

# Set fzf to always use bat for previews
# export FZF_DEFAULT_OPTS="--preview 'bat --color=always {}'"

if [ "$isUbuntu" = "true" ]; then
		# Ubuntu Debian per /usr/share/doc/fzf/README.Debian
		source /usr/share/doc/fzf/examples/key-bindings.bash
		# Should be unnecessary in later apt fzf versions
		source ~/Code/External/fzf/shell/completion.bash
		# Change fzf find files command from default find to fd-find
		# fd-find is called fdfind on Ubuntu due to a file name clash
		# per apt-cache show fd-find
		alias fd="fdfind"
		# Find:
		# --type f = files
		# --hidden = include hidden files
		export FZF_DEFAULT_COMMAND='fdfind --hidden --type f'
fi

if [ "$isFedora" = "true" ]; then
		# FZF mappings and options
		[ -f /usr/share/fzf/shell/key-bindings.bash ] && source /usr/share/fzf/shell/key-bindings.bash
    source /usr/share/fzf/shell/key-bindings.bash
		export FZF_DEFAULT_COMMAND='fd --hidden --type f'

		# SSH key management with i3
		# Prompt once for SSH keys, then remember for rest of user's session
		# Work in terminal and non terminal environments
		# https://wiki.archlinux.org/title/SSH_keys#Keychain
fi

# If running i3, eval the keychain
# see man keychain for other shells and additional certificates
if pgrep -x "i3" > /dev/null
then
		eval $(keychain --eval --quiet id_ed25519 id_rsa)
fi

# oc - Openshift CLI
# if oc command exists, source completions
if command -v oc &> /dev/null
then
		source <(oc completion bash)
fi

# Nix
# added by Nix single user installer in .bash_profile, moved here
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi 

# Starship
eval "$(starship init bash)"

# Go to fish shell on non-login shells
fish
