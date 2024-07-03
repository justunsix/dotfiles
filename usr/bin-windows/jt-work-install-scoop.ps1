scoop bucket add java
scoop bucket add extras
scoop bucket add nerd-fonts
# Lists of scoop packages to install
$programsList = @(
    ## Package Management
    "topgrade",
    ## File Management
    "git",
    "extras/git-credential-manager",
    "extras/lazygit",
    "bleachbit",
    "ccleaner",
    "fzf",
    "fd",
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
    # "obs-studio",
    # "gimp",
    # "inkscape",
    ## Web
    "firefox",
    ## Fonts
    "nerd-fonts/JetBrains-Mono",
    ## OCR
    "capture2text",
    ## Shell / Terminal
    "pwsh",
    "zoxide",
    "gsudo",
    "mprocs",
    "dog",
    "extras/wezterm",
    "extras/carapace-bin",
    "cygwin",
    "nu",    
    ### Prompt
    "starship",
    ## Office, Productivity
    "peazip",
    "plantuml",    
    "draw.io",
    "autohotkey",
    ## Media
    "vlc",
    ## Security    
    "keepassxc",
    ## DevOps    
    "vscode",
    "pycharm",
    "helix",
    ### Languages    
    "make", 
    #### Python
    "python",
    "extras/miniconda3",    
    #### Java
    "java/temurin-lts-jdk",    
    "maven",
    #### JavaScript / TypeScript
    "nodejs",
    #### IAC
    "terraform",      
    #### VS Code Polyglot literate programming
    "dotnet-sdk",
    #### k8s
    "kubectl",
    "azure-kubelogin",
    "k9s",
    #### cloud
    "azure-ps",
    "azure-cli",    
    #### SQL
    "usql"    
    ### Emacs and Supporting Programs
    "emacs",
    "ripgrep",
    "pandoc",
    "mremoteng",
    "zoxide",
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
    "gcc",    
    #### Astronvim Supporting Programs
    "win32yank",
    "tree-sitter"
)

# Convert programs to be installed by scoop into a single space-separated string
$programs = $programsList -join " "

# Install list of programs
# Use Invoke-Expression to execute as if on command line
Invoke-Expression "scoop install $programs"
