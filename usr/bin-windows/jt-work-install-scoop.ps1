scoop bucket add java
scoop bucket add extras
scoop bucket add nerd-fonts
scoop bucket add versions
# Lists of scoop packages to install
$programsList = @(
    ## System
    "btop",
    ### git, including bash
    "git",
    ## Package Management
    "topgrade",
    ## File Management
    "extras/git-credential-manager",
    "uutils-coreutils",
    "eza",
    "bat",
    "less",
    "zoxide",
    ### lg optional for nvim frameworks
    "extras/lazygit",
    "bleachbit",
    "fzf",
    "extras/television",
    ### fd optional for nvim, emacs frameworks
    "fd",
    #### rg for nvim (optional), emacs (required) frameworks
    "ripgrep",
    "duf",
    "extras/everything",
    "kondo",
    "extras/dolphin",
    "broot",
    ### yazi file manager and supporting programs
    "yazi",
    #### yazi optional dependencies
    "unar",
    "jq",
    "poppler",
    # "bottom",
    ## Media
    # "figma",
    "extras/audacity",
    # "obs-studio",
    # "gimp",
    # "inkscape",
    ## Web
    "firefox",
    ## Fonts
    ### For general, emacs use
    "nerd-fonts/JetBrains-Mono",
    ### Patched for alacritty
    "nerd-fonts/JetBrainsMono-NF-Mono",
    ## OCR
    "capture2text",
    ## Shell / Terminal
    "pwsh",
    "zoxide",
    "gsudo",
    "doggo",
    "mprocs",
    "versions/wezterm-nightly"
    "extras/carapace-bin",
    "atuin",
    # "cygwin",
    "msys2",
    "nu",
    ### Prompt
    "starship",
    ## Office, Productivity
    "peazip",
    "plantuml",
    ### graphiz is needed by plantuml for diagram generation
    "graphviz",
    "draw.io",
    "autohotkey",
    "okular",
    "espanso",
    ## Media
    "vlc",
    ## Security
    "extras/gpg4win"
    "keepassxc",
    "sops",
    ## DevOps
    "vscode",
    "mremoteng",
    "helix",
    ### Tool, runtime, environment management
    "mise",
    ### Languages
    "make", 
    #### Python
    "python",
    # Replace pipx with uv
    # "pipx",
    ##### Python package, project, tool manager
    "uv",
    ##### Python LSP
    "ty",
    #### Java
    # "java/temurin-lts-jdk",
    # "maven",
    #### JavaScript / TypeScript
    "nodejs",
    #### XML
    ##### XML Formatter, linter
    "extras/xmllint",
    ##### XML LSP
    "lemminx",
    #### IAC
    "terraform",
    #### k8s
    "kubectl",
    "azure-kubelogin",
    "k9s",
    "stern",
    "kubectx",
    "kubens",
    "extras/headlamp",
    #### cloud
    "azure-ps",
    "azure-cli",
    #### SQL
    "usql"
    #### sh - for use with Emacs 
    "shfmt",
    ### Emacs and Supporting Programs
    "emacs",
    "pandoc",
    #### For use with Emacs diff, grep, gzip for undo
    "diffutils",
    #### Shellcheck used by Emacs Flymake for shell scripts by default - Emacs 29.1
    "shellcheck",
    #### Used by org-download for clipboard pasting to org
    "imagemagick",
    #### Emacs Doom Framework
    "llvm",
    #### Spell check
    "aspell",
    ### Neovim and Supporting Programs
    "neovim",
    #### for telescope fzf native
    "cmake",
    #### For Neovim and Emacs org-roam SQLite
    "gcc",
    #### nvim-treesitter
    "tree-sitter",
    ## Data Science, Artificial Intelligence (AI)
    "opencode"
)

# Convert programs to be installed by scoop into a single space-separated string
$programs = $programsList -join " "

# Install list of programs
# Use Invoke-Expression to execute as if on command line
Invoke-Expression "scoop install $programs"

# Hold msys2 update and update only within msys2 rolling updates
scoop hold msys2

## Install LSPs, formatters, linters for use
## with helix editor
uv tool install pyright
uv tool install ruff
uv tool install black
uv tool upgrade --all
scoop install main/marksman
## npm installs
npm install -g prettier
npm install -g bash-language-server
npm update -g

# Yazi theme
ya pkg add yazi-rs/flavors:catppuccin-mocha

# Television channels update
tv update-channels
