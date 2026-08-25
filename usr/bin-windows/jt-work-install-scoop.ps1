scoop bucket add java
scoop bucket add extras
scoop bucket add nerd-fonts
# Lists of scoop packages to install
$programsList = @(
    ## System
    "btop",
    ### git, including bash
    "git",
    #### git diff
    "delta",
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
    ### yazi file manager and supporting programs
    "yazi",
    #### yazi optional dependencies
    "ffmpeg",
    "7zip",
    "jq",
    "poppler",
    # "resvg",
    # "bottom",
    ## Media
    # "figma",
    "extras/audacity",
    # "obs-studio",
    # "gimp",
    # "inkscape",
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
    "zellij",
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
    #### PDF
    "okular",
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
    #### Go Lang
    "go",
    #### Java
    # "java/temurin-lts-jdk",
    # "maven",
    #### JavaScript / TypeScript
    "nodejs",
    "pnpm",
    #### XML
    ##### XML Formatter, linter
    "extras/xmllint",
    ##### XML LSP
    "lemminx",
    ### Terraform
    "terraform",
    ### Kubernetes (k8s)
    "kubectl",
    "azure-kubelogin",
    "k9s",
    "stern",
    "kubectx",
    "kubens",
    "extras/headlamp",
    #### cloud
    "azure-cli",
    #### SQL
    "usql"
    #### sh - for use with Emacs 
    "shfmt",
    ### Emacs and Supporting Programs
    "emacs",
    #### Emacs 31.1 no longer bundles ctags, use Univerval Ctags
    "universal-ctags"
    "pandoc",
    #### For use with Emacs diff, grep, gzip for undo
    "diffutils",
    #### Shellcheck used by Emacs Flymake for shell scripts by default - Emacs 29.1
    "shellcheck",
    #### Used by org-download for clipboard pasting to org and yazi for previews
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

# Install Firefox separately due to manifest issues
scoop install firefox

# Install list of programs
# Use Invoke-Expression to execute as if on command line
Invoke-Expression "scoop install $programs"

# Hold msys2 update and update only within msys2 rolling updates
scoop hold msys2

## Install LSPs, formatters, linters for use
 
## For Helix editor
uv tool install pyright
uv tool install ruff
uv tool install black
uv tool upgrade --all
scoop install main/marksman
### npm installs
pnpm setup
pnpm add -g prettier
pnpm add -g bash-language-server
pnpm add -g vscode-langservers-extracted
pnpm add -g dockerfile-language-server-nodejs

pnpm outdated -g
pnpm update -g --latest

### Go installs
#### For Helix editor
go install github.com/reteps/dockerfmt@latest
### For Doom Emacs
go install github.com/jessfraz/dockfmt@latest

# Yazi packages
## Yazi theme
ya pkg add yazi-rs/flavors:catppuccin-mocha
## MIME type detector
ya pkg add yazi-rs/plugins:mime-ext
## Preview toggle
ya pkg add yazi-rs/plugins:toggle-pane

# Television channels update
tv update-channels
