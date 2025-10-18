# Aliases

# git
alias jgs="jt-gs.sh"
alias gglrs="gfold | rg -e unclean -e unpushed || true"
alias ggsc="jt-gc.sh"
alias jvcs='vcs status $HOME/Code | rg -e modified -e === -e Untracked'
alias ggs='git status'
alias ggd='git diff'

# todotxt
alias t='todo.sh -d $HOME/.config/todotxt-cli/todo.cfg'
alias to='todo.sh -d $HOME/.config/todotxt-cli/todo-work.cfg'

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

# eza
alias exa="eza"
alias eza="eza --icons -lh --group-directories-first --git"
alias e="eza -alh"

## Changing "ls" to "exa"
# alias ls='eza -al --color=always --group-directories-first' # my preferred listing
# alias la='eza -a --color=always --group-directories-first'  # all files and dirs
# alias ll='eza -l --color=always --group-directories-first'  # long format
# alias lt='eza -aT --color=always --group-directories-first' # tree listing
# alias l.='eza -a | egrep "^\."'

# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# fzf
# shellcheck disable="SC2139"
alias ff='fzf --preview "bat --style=numbers --color=always --line-range :500 {}" | xargs "$EDITOR"'
alias fm='fmake.sh'
# shellcheck disable=SC2142
alias fkill='ps -ef | fzf | awk '\''{print $2}'\'' | xargs kill -9'

# Lazygit
alias gg='lazygit'

# rofi UI
alias rofi='rofi -lines 12 -padding 18 -width 60 -location 0 -show drun -sidebar-mode -columns 3'
