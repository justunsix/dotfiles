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

########################################
########################################
# --- My Customizations ---

################################
#
# * Variables & Environment Variables
#
################################

# Source overlay variables, aliases
if [ -f "$HOME/.env" ]; then
    source "$HOME/.env"
    # aliases
    source "$HOME/.env-aliases"
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

# Moved to .bash_profile, as Ubuntu Gnome reads from there
# export QT_QPA_PLATFORMTHEME="qt5ct"

################################
# * PATH

if [ -d "$HOME/usr/bin/phantomjs-2.1.1-linux-x86_64/bin" ] ;
  then PATH="$HOME/usr/bin/phantomjs-2.1.1-linux-x86_64/bin:$PATH"
fi

if [ -d "$HOME/.nix-profile/bin" ] ;
  then PATH="$HOME/.nix-profile/bin:$PATH"
fi

if [ -d "$HOME/usr/bin/todotxt-cli" ] ;
  then PATH="$HOME/usr/bin/todotxt-cli:$PATH"
fi

if [ -d "$HOME/usr/bin" ] ;
  then PATH="$HOME/usr/bin:$PATH"
fi

if [ "$isWSLUbuntu" = "true" ] ;
    then PATH="$HOME/Code/dotfiles/usr/bin:$PATH"
fi

export PATH="$HOME/.local/bin::$PATH"

################################
# * Other Variables

# Set EDITOR environment variable
export EDITOR="vim"

# "less" as manpager
# other options could be bat, vim, nvim
# examples at https://gitlab.com/dwt1/dotfiles/-/blob/master/.bashrc
export MANPAGER="less"

export EMACS_SERVER_FILE="$HOME/.emacs.d/server/server"

# nix home-manager
if [ -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
	. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
fi

# ** XDG
# Check if they are empty and set them to default values

if [ -z "$XDG_CONFIG_HOME" ] ; then
    export XDG_CONFIG_HOME="$HOME/.config"
fi
if [ -z "$XDG_DATA_HOME" ] ; then
    export XDG_DATA_HOME="$HOME/.local/share"
fi
if [ -z "$XDG_CACHE_HOME" ] ; then
    export XDG_CACHE_HOME="$HOME/.cache"
fi

################################
#
# Programs
#
################################

# Check programs are installed before configuring them.
# Conditionals allow reusing this .bashrc on multiple systems

## cat(1) clone with syntax highlighting and git integration
# alias bat="batcat"

# Node Version Manager (NVM)
export NVM_DIR="$HOME/.nvm"

# If NVM binary exists, load it
if [ -f "$NVM_DIR/nvm.sh" ]; then
    # This loads nvm
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    # This loads nvm bash_completion
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

    # Use default node version is none is set
    if [ -f .nvmrc ]; then
        nvm use
    else
        nvm use default
    fi
fi

## Emacs
# Ensure in emacs (start-server) is done in init or on emacs command line
# Find emacs server socks using:
# lsof -c emacs | grep server | tr -s " "
# Default on Linux is "/run/user/1000/emacs/server"
# alias emacsclient="emacsclient --socket-name="/run/user/1000/emacs/server"

## Haskell - Glasgow Haskell Compiler
# ghcup-env
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"

## fzf
# Set fzf to always use bat for previews
# export FZF_DEFAULT_OPTS="--preview 'bat --color=always {}'"
# If fzf command is on system, configure it
if command -v fzf >/dev/null; then
    if [ "$isUbuntu" = "true" ]; then
        # Ubuntu Debian per /usr/share/doc/fzf/README.Debian
        source /usr/share/doc/fzf/examples/key-bindings.bash
        # Should be unnecessary in later apt fzf versions
        source "$HOME/Code/External/fzf/shell/completion.bash"
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
    fi
fi

## Keychain
# SSH key management with i3 or WSL Ubuntu
# Prompt once for SSH keys, then remember for rest of user's session
# Work in terminal and non terminal environments
# https://wiki.archlinux.org/title/SSH_keys#Keychain
# see man keychain for other shells and additional certificates
if command -v keychain >/dev/null; then
    # If i3 is running or WSL Ubuntu, run keychain
    if pgrep -x "i3" >/dev/null || [ "$isWSLUbuntu" = "true" ]; then
        eval "$(keychain --eval --quiet id_ed25519)"
        # optionally include id_rsa
        # eval $(keychain --eval --quiet id_ed25519)
    fi
fi

## oc - Openshift CLI
# if oc command exists, source completions
if command -v oc &>/dev/null; then
    source <(oc completion bash)
fi

## Nix
# added by Nix single user installer in .bash_profile, moved here
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
    . ~/.nix-profile/etc/profile.d/nix.sh
fi

## Starship Prompt
if command -v starship &>/dev/null; then
    # Starship
    eval "$(starship init bash)"
fi

## Cargo, Rust package manager
if [ -d "$HOME/.cargo" ]; then
    # rust tools
    . "$HOME/.cargo/env"
fi

## Zoxide - smarter cd
if command -v zoxide &>/dev/null; then
    # zoxide - smarter cd
    eval "$(zoxide init bash)"
fi

## broot - interactive tree view
if command -v broot &>/dev/null; then
    # Run br first time to generate default configuration
    source "$HOME/.config/broot/launcher/bash/br"
fi

## atuin - history
if command -v atuin &>/dev/null; then
    eval "$(atuin init bash)"
fi

## carapace - shell completion
if command -v carapace &>/dev/null; then
    export CARAPACE_BRIDGES='zsh,fish,bash,inshellisense' # optional
    source <(carapace _carapace)
fi

## conda - package manager
## Detect conda-shell installed by Nix
if command -v conda-shell >/dev/null; then

		# >>> conda initialize >>>
		# !! Contents within this block are managed by 'conda init' !!
		__conda_setup="$('/home/justin/.conda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
		if [ $? -eq 0 ]; then
				eval "$__conda_setup"
		else
				if [ -f "/home/justin/.conda/etc/profile.d/conda.sh" ]; then
						. "/home/justin/.conda/etc/profile.d/conda.sh"
				else
						export PATH="/home/justin/.conda/bin:$PATH"
				fi
		fi
		unset __conda_setup
		# <<< conda initialize <<<

fi

## phantonjs
## Correct issue per https://github.com/ariya/phantomjs/issues/15449
if command -v phantomjs &>/dev/null; then
    export OPENSSL_CONF=/etc/ssl~
fi

if command -v broot &>/dev/null; then
    source $HOME/.config/broot/launcher/bash/br
fi

################################
#
# Functions
#
################################


# ** Functions from https://gitlab.com/dwt1/dotfiles/-/blob/master/.bashrc

### ARCHIVE EXTRACTION
# From
# usage: ex <file>
jt-extract ()
{
  if [ -f "$1" ] ; then
    case $1 in
      *.tar.bz2)   tar xjf "$1"   ;;
      *.tar.gz)    tar xzf "$1"   ;;
      *.bz2)       bunzip2 "$1"   ;;
      *.rar)       unrar x "$1"   ;;
      *.gz)        gunzip "$1"    ;;
      *.tar)       tar xf "$1"    ;;
      *.tbz2)      tar xjf "$1"   ;;
      *.tgz)       tar xzf "$1"   ;;
      *.zip)       unzip "$1"     ;;
      *.Z)         uncompress "$1";;
      *.7z)        7z x "$1"      ;;
      *.deb)       ar x "$1"      ;;
      *.tar.xz)    tar xf "$1"    ;;
      *.tar.zst)   unzstd "$1"    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

### navigation with cd
### move up in directories
jt-up () {
  local d=""
  local limit="$1"

  # Default to limit of 1
  if [ -z "$limit" ] || [ "$limit" -le 0 ]; then
    limit=1
  fi

  for ((i=1;i<=limit;i++)); do
    d="../$d"
  done

  # perform cd. Show error if cd fails
  if ! cd "$d"; then
    echo "Couldn't go up $limit dirs.";
  fi
}


################################
#
# Start Up
#
################################

if command -v fish >/dev/null; then
    # Go to fish shell on non-login shells
    fish
fi
