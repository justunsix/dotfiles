# Nushell Config File
#
# version = "0.101.0"
$env.config.color_config = {
    separator: white
    leading_trailing_space_bg: { attr: n }
    header: green_bold
    empty: blue
    bool: light_cyan
    int: white
    filesize: cyan
    duration: white
    date: purple
    range: white
    float: white
    string: white
    nothing: white
    binary: white
    cell-path: white
    row_index: green_bold
    record: white
    list: white
    closure: green_bold
    glob:cyan_bold
    block: white
    hints: dark_gray
    search_result: { bg: red fg: white }
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
        fg: white
        bg: red
        attr: b
    }
}

#####################
# My Configurations #
#####################

#####################
# Aliases
alias lg = lazygit
alias e = eza -alh

#####################
# My Custom Commands
# Per https://www.nushell.sh/book/custom_commands.html

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
    vcs status $codedir | rg -e modified -e ===
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
# Shell assitance

# Starship prompt
use ~/.cache/starship/init.nu

# Zoxide
## temp fix 2024-01-06 for older zoxide version in case of "did you mean 'let-env' error
### https://github.com/ajeetdsouza/zoxide/issues/654#issuecomment-1875476154
### run in nushell:
### open ~/.zoxide.nu | str replace --all 'def-env' 'def --env' | save -f ~/.zoxide-fixed.nu
### open ~/.zoxide-fixed.nu | str replace --all '$rest' '...$rest' | save -f ~/.zoxide-fixed.nu;
### source ~/.zoxide-fixed.nu
### Fixed as of 2024-02-13, confirmed fixed on Windows scoop and Nix unstable
source ~/.zoxide.nu

# Carapace - Shell completions
source ~/.cache/carapace/init.nu

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
