# Default Nushell Environment Config File
# These "sensible defaults" are set before the user's `env.nu` is loaded
#
# version = "0.101.0"

$env.PROMPT_COMMAND = $env.PROMPT_COMMAND? | default {||
    let dir = match (do -i { $env.PWD | path relative-to $nu.home-path }) {
        null => $env.PWD
        '' => '~'
        $relative_pwd => ([~ $relative_pwd] | path join)
    }

    let path_color = (if (is-admin) { ansi red_bold } else { ansi green_bold })
    let separator_color = (if (is-admin) { ansi light_red_bold } else { ansi light_green_bold })
    let path_segment = $"($path_color)($dir)(ansi reset)"

    $path_segment | str replace --all (char path_sep) $"($separator_color)(char path_sep)($path_color)"
}

$env.PROMPT_COMMAND_RIGHT = $env.PROMPT_COMMAND_RIGHT? | default {||
    # create a right prompt in magenta with green separators and am/pm underlined
    let time_segment = ([
        (ansi reset)
        (ansi magenta)
        (date now | format date '%x %X') # try to respect user's locale
    ] | str join | str replace --regex --all "([/:])" $"(ansi green)${1}(ansi magenta)" |
        str replace --regex --all "([AP]M)" $"(ansi magenta_underline)${1}")

    let last_exit_code = if ($env.LAST_EXIT_CODE != 0) {([
        (ansi rb)
        ($env.LAST_EXIT_CODE)
    ] | str join)
    } else { "" }

    ([$last_exit_code, (char space), $time_segment] | str join)
}

#####################
# My Configurations #
#####################

# Do not show welcome message
$env.config.show_banner = false

# Set Helix as default editor
$env.EDITOR = "hx"

# Set NVIM Framework to use
$env.NVIM_APPNAME = 'nvim-lazyvim'

# Starship prompt
mkdir ~/.cache/starship
starship init nu | save -f ~/.cache/starship/init.nu

# Zoxide - directory navigation
zoxide init nushell | save -f ~/.zoxide.nu

# Carapace - completions
$env.CARAPACE_BRIDGES = 'zsh,fish,bash,inshellisense' # optional
mkdir $"($nu.cache-dir)"
carapace _carapace nushell | save --force $"($nu.cache-dir)/carapace.nu"

# Atuin - shell history
mkdir ~/.local/share/atuin/
atuin init nu | save -f ~/.local/share/atuin/init.nu

#####################
# Old Fixes         #
#####################

## Temporary fix for Nushell deprecating --redirect-stderr
## https://github.com/atuinsh/atuin/pull/1913/commits/4c564aca2f385d38f26c13f5b4aeeee318dce0d4
## open ~/.local/share/atuin/init.nu | str replace --all 'run-external --redirect-stderr atuin search' 'run-external atuin ## search' | save -f ~/.local/share/atuin/init.nu;
## open ~/.local/share/atuin/init.nu | str replace --all '| complete | $in.stderr | str substring ..-1)' 'e>| str trim)' | ## save -f ~/.local/share/atuin/init.nu;
