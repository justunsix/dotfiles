# Aliases

# git
alias jgs="jt-gs.sh"
alias jgt="gfold | rg -e unclean -e unpushed"
alias jgc="jt-gc.sh"
alias jvcs="vcs status $HOME/Code | rg -e modified -e ==="

# todotxt
alias t="todo.sh -d $HOME/.config/todotxt-cli/todo.cfg"
alias to="todo.sh -d $HOME/.config/todotxt-cli/todo-work.cfg"

# cd
alias ..="cd .."
alias ...="cd ../.."
alias cd1="cd .."
alias cd2="cd ../.."
alias cd3="cd ../../.."
alias cd4="cd ../../../.."

# emacs
## Simpler config for terminal use
alias emacsnw="emacs -Q -nw -l ~/Code/dotfiles/.config/emacs/setup/minimal.el"

# exa
alias exa="eza"
alias eza="eza --icons -lh --group-directories-first"

## Changing "ls" to "exa"
# alias ls='eza -al --color=always --group-directories-first' # my preferred listing
# alias la='eza -a --color=always --group-directories-first'  # all files and dirs
# alias ll='eza -l --color=always --group-directories-first'  # long format
# alias lt='eza -aT --color=always --group-directories-first' # tree listing
# alias l.='eza -a | egrep "^\."'

# grep
# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# Lazygit
alias lg='lazygit'

# yt-dlp
# List formats for a URL, Check URL is downloadable
alias ytf="yt-dlp -F URL"
# Download file with at best 720p resolution
alias yt720="yt-dlp -S 'res:720'"
