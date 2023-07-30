# Aliases

# git
alias jgs="jt-gs.sh"
alias jgt="jt-gt.sh"
alias jgc="jt-gc.sh"

# todotxt
alias t="todo.sh -d $HOME/.config/todotxt-cli/todo.cfg"
alias to="todo.sh -d $HOME/.config/todotxt-cli/todo-work.cfg"

# cd
alias ..="cd .."
alias ...="cd ../.."
alias cd1="cd .."
alias cd2="cd ../.."
alias cd3="cd ../../.."

# emacs
## Simpler config for terminal use
alias emacsnw="emacs -Q -nw -l ~\.config\emacs\setup\minimal.el"

# exa
alias exa="exa --icons -lh --group-directories-first"

## Changing "ls" to "exa"
# alias ls='exa -al --color=always --group-directories-first' # my preferred listing
# alias la='exa -a --color=always --group-directories-first'  # all files and dirs
# alias ll='exa -l --color=always --group-directories-first'  # long format
# alias lt='exa -aT --color=always --group-directories-first' # tree listing
# alias l.='exa -a | egrep "^\."'

# grep
# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# yt-dlp
# List formats for a URL, Check URL is downloadable
alias ytf="yt-dlp -F URL"
# Download file with at best 720p resolution
alias yt720="yt-dlp -S 'res:720'"
