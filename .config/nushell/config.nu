# Nushell Config File
#
# version = "0.111.1"
$env.config.color_config = {
    separator: default
    leading_trailing_space_bg: { attr: n }
    header: green_bold
    empty: blue
    bool: light_cyan
    int: default
    filesize: cyan
    duration: default
    datetime: purple
    range: default
    float: default
    string: default
    nothing: default
    binary: default
    cell-path: default
    row_index: green_bold
    record: default
    list: default
    closure: green_bold
    glob:cyan_bold
    block: default
    hints: dark_gray
    search_result: { bg: red fg: default }
    shape_binary: purple_bold
    shape_block: blue_bold
    shape_bool: light_cyan
    shape_closure: green_bold
    shape_custom: green
    shape_datetime: cyan_bold
    shape_directory: cyan
    shape_external: cyan
    shape_externalarg: green_bold
    shape_external_resolved: light_yellow_bold
    shape_filepath: cyan
    shape_flag: blue_bold
    shape_float: purple_bold
    shape_glob_interpolation: cyan_bold
    shape_globpattern: cyan_bold
    shape_int: purple_bold
    shape_internalcall: cyan_bold
    shape_keyword: cyan_bold
    shape_list: cyan_bold
    shape_literal: blue
    shape_match_pattern: green
    shape_matching_brackets: { attr: u }
    shape_nothing: light_cyan
    shape_operator: yellow
    shape_pipe: purple_bold
    shape_range: yellow_bold
    shape_record: cyan_bold
    shape_redirection: purple_bold
    shape_signature: green_bold
    shape_string: green
    shape_string_interpolation: cyan_bold
    shape_table: blue_bold
    shape_variable: purple
    shape_vardecl: purple
    shape_raw_string: light_purple
    shape_garbage: {
        fg: default
        bg: red
        attr: b
    }
}
 
########################################################
# My Custom Commands                                   #
# Per https://www.nushell.sh/book/custom_commands.html #
# ######################################################

# Kill given process or select process(es) to kill
#
# Process to kill is optional. If not provided
# select the process(es) to be stopped.
def fkill [
    process = "": string   # Process to kill
    ] {
    if ($process | is-empty) {
        echo "Kill a process with fkill <process name> or fkill to select a process"

        # Get process and multi select with fzf
        let processes = ps | select pid name | each {|it| $'($it.pid) ($in.name)'}
        let selected = ($processes | to text | fzf -m --height 40% --reverse --inline-info --prompt "Select process(es) to kill: ")

        if ($selected | is-empty) {
            print "No process selected."
        } else {
           print "Killing processes" $selected
           # Extract PIDs from selected lines and kill them
            $selected
            | lines
            | each {|line| ($line | split column " " | get 0 | get column1) }
            | each {|pid|
                ^kill $pid}
        }
    } else {
        ps | where name =~ $process | first | kill $in.pid -f
    }
}
 
# Get Makefile tasks in directory, pick and run task
def fm [] {
    # Check if fzf is installed
    if (which fzf | is-empty) {
        print "fzf is not installed. Please install it to use this script."
        return
    }

    # Check if Makefile exists
    if not (["Makefile"] | path exists | get 0) {
        print "No Makefile found in the current directory."
        return
    }

    # Extract make targets with `##` help comments (like `target: ## description`)
    let targets = open Makefile
        | lines
        | where ($it =~ '^[a-zA-Z0-9][^:]*:.*##')
        | each {|line| $line | split row ":" | get 0 | str trim }
        | uniq

    # Pass targets to fzf for selection
    let selected_target = ($targets | to text | fzf --height 40% --reverse --inline-info --prompt "Select a target: ")

    # Run make with the selected target
    if not ($selected_target | is-empty ) {
        print $"Executing make ($selected_target)..."
        ^make $selected_target
    } else {
        print "No target selected."
    }
    
}


# yazi directory change
def --env y [...args] {
	let tmp = (mktemp -t "yazi-cwd.XXXXXX")
	yazi ...$args --cwd-file $tmp
	let cwd = (open $tmp)
	if $cwd != "" and $cwd != $env.PWD {
		cd $cwd
	}
	rm -fp $tmp
}

# Check Git repositories for pending changes
def jvcs [] {
    let codedir = [$env.HOME, '/Code'] | str join
    vcs status $codedir | rg -e modified -e === -e Untracked
}

# Check Git repositories with unpushed or un committed changes
def jgt [] {
    gfold | rg -e unclean -e unpushed | complete
}

# Stages, commits, and pushes Git changes with a provided commit message or "autocommit message" if no message is provided
def jgc [
  message = "auto commit": string   # Commit message
  ] {
    # Commit with the provided message
    git commit -am $message

    # Push to the current branch
    git push
}

# Search for string in files and open in editor
def fgrep [
    stringToSearch = 'todo': string # Search term
    --ext: string = "*", # Search on files with these extensions
    --vim, # If it should open in neovim
] {

    # Search case insensitive with rg including hidden files except .git dir,
    # and including extension in glob to search, then
    # filter file list with fzf and get filename with cut
    let $result = rg -i $stringToSearch --hidden -g'!.git' -g $"*.($ext)" | fzf | cut -d':' -f 1
    if ($vim) {
        nvim $result
    } else {
        ^$env.EDITOR $result
    }
}

# Check Git status for multiple repositories
# Usage with multiple directories on Windows: jgt "~\\Code,T:\\OtherProjects"
# Fix per https://github.com/nushell/nushell/pull/12232
# Run with jgt 'C:/Users/username/Code'
# def jgt [
#     directories = "~/Code"              # Parent directories of the reposities to check separated by commas
#     --auto (-a) : string                # Whether to automatically commit and push changes
# ] {

#     # Assuming directory is a string of directories separated by commas
#     # Split the string into an array of directories
#     let dirs = $directories | split row ","

#     # Iterate through the directories
#     for $directory in $dirs {
#         # Iterate through child directories of $directory
#         for $it in (ls $directory -a | where type == dir | get name) {

#                 cd $it

#                 # Check if it's a git repository, only record standard out messages
#                 # Otherwise if an error, it is not a git repository
#                 let git_status_check = do { git status --porcelain } | complete

#                 # Check if it's a git repository
#                 if ( $git_status_check.exit_code == 0) {
#                     # if stdout is not empty
#                     if ($git_status_check.stdout | is-empty) {
#                         # no changes found
#                     } else {
#                         # Changes found, print the repository name and status
#                         echo $"(ansi red_bold)---(ansi reset)" $it
#                         # echo $git_status_check.stdout
#                         # Output changed or new git files
#                         echo $git_status_check.stdout

#                         if $auto == "true" {
#                             # Commit and push the changes
#                             jgc
#                         }
#                     }
#                 }
#         }

#     }
# }

#####################
# Aliases
alias e = eza -alh
alias k = kubectl
alias mr = mise run
alias ggs = git s
alias ggd = git diff

## todotxt
alias t = todo.sh -d ~/.config/todotxt-cli/todo.cfg
alias to = todo.sh -d ~/.config/todotxt-cli/todo-work.cfg

## Aliases inspired by LazyVim and Doom Emacs
### find file, open with editor
alias ff = ^$env.EDITOR (fd --hidden --exclude .git | fzf)
alias ffn = nvim (fd --hidden --exclude .git | fzf)
### git status
alias gg = lazygit
### git status commit
alias ggsc = jgc
alias ggf = git pull
alias ggl = git l
### git list repositories, status of repositories
alias gglrs = jgt
### git list repositories, pull all repositories
alias gglrf = topgrade --only git_repos

#####################
# Shell assitance

# Starship prompt
use ~/.cache/starship/init.nu

# Zoxide
source ~/.zoxide.nu

# Carapace - Shell completions
source $"($nu.cache-dir)/carapace.nu"

# mise
use ($nu.default-config-dir | path join mise.nu)

# Conditional Sourcing based on operating system
## Conditional sourcing
## per https://www.nushell.sh/blog/2023-09-19-nushell_0_85_0.html#improvements-to-parse-time-evaluation
const CONFIG_WINDOWS = "~/AppData/Roaming/nushell/config-windows.nu"
const CONFIG_NIX = "~/.config/nushell/config-nix.nu"

const CONFIG_ACTUAL = if $nu.os-info.name == "windows" {
    $CONFIG_WINDOWS
} else {
    $CONFIG_NIX
}

source $CONFIG_ACTUAL

