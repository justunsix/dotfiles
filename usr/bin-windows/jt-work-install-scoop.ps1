scoop bucket add java
scoop bucket add extras
scoop bucket add nerd-fonts
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
    "ccleaner",
    "fzf",
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
    "figma",
    # "obs-studio",
    # "gimp",
    # "inkscape",
    ## Web
    "firefox",
    ## Fonts
    "nerd-fonts/JetBrainsMono-NF-Mono",
    ## OCR
    "capture2text",
    ## Shell / Terminal
    "pwsh",
    "zoxide",
    "gsudo",
    "doggo",
    "mprocs",
    "extras/wezterm",
    "extras/carapace-bin",
    # "cygwin",
    "msys2",
    "nu",
    ### Prompt
    "starship",
    ## Office, Productivity
    "peazip",
    "plantuml",
    "draw.io",
    "autohotkey",
    "okular",
    "espanso",
    ## Media
    "vlc",
    ## Security
    "extras/gpg4win"
    "keepassxc",
    ## DevOps    
    "vscode",
    "mremoteng",
    # "pycharm",
    "helix",
    ### Languages    
    "make", 
    #### Python
    "python",
    # "extras/miniconda3",
    # Replace pipx with uv
    # "pipx",
    ##### Python package, project, tool manager
    "uv",
    #### Java
    # "java/temurin-lts-jdk",    
    # "maven",
    #### JavaScript / TypeScript
    "nodejs",
    #### IAC
    "terraform",      
    #### VS Code Polyglot literate programming
    # "dotnet-sdk",
    #### k8s
    "kubectl",
    "azure-kubelogin",
    "k9s",
    "stern",
    "kubectx",
    #### cloud
    "azure-ps",
    "azure-cli",
    #### SQL
    "usql"    
    ### Emacs and Supporting Programs
    "emacs",
    "pandoc",
    #### For use with Emacs diff
    "diffutils",
    #### Shellcheck used by Emacs Flymake for shell scripts by default - Emacs 29.1
    "shellcheck",
    #### Used by org-download for clipboard pasting to org
    "imagemagick",
    #### Emacs Doom Framework
    "llvm",
    ### Neovim and Supporting Programs
    "neovim",
    #### for telescope fzf native
    "cmake",
    #### For Neovim and Emacs org-roam SQLite
    "gcc"
    #### Astronvim Supporting Programs
    # "win32yank",
    # "tree-sitter"
)

# Convert programs to be installed by scoop into a single space-separated string
$programs = $programsList -join " "

# Install list of programs
# Use Invoke-Expression to execute as if on command line
Invoke-Expression "scoop install $programs"

# Python based installs
uv tool install visidata
uv tool install vcstool

# Yazi theme
ya pack -a yazi-rs/flavors:catppuccin-mocha
