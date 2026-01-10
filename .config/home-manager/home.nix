{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "justin";
  home.homeDirectory = "/home/justin";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # hello

    # ------------------------------------------------------------
    # Main Packages

    # Shell
    fish
    nushell
    starship
    bat
    atuin
    carapace
    rofi
    fzf
    television
    ## rg optional for nvim and emacs frameworks
    ripgrep
    doggo
    grex
    # Fonts
    iosevka
    ## Default font for Doom Emacs
    jetbrains-mono
    ## Starship prompt, eza nerd font
    nerd-fonts.fira-code
    source-code-pro
    nerd-fonts.jetbrains-mono
    # File Management
    gfold
    img2pdf
    duf
    stow
    yazi
    lazygit
    kondo
    broot
    # Web
    lynx
    # Productivity and Tasks
    todo-txt-cli
    # System and Monitors
    btop
    procs
    espanso
    ## bottom aka btm
    ## bottom
    # Terminal
    tmux
    mprocs
    ## Data
    visidata
    ## sd
    ## Former rust cargo installs
    dust
    eza
    monolith
    topgrade
    zoxide
    # DevOps
    asdf-vm
    ### Doom Emacs requirements
    fd
    ## Other Editors
    neovim
    helix

    # Languages
    ## Language Servers installs, LSP, Linters
    ## https://github.com/helix-editor/helix/wiki/How-to-install-the-default-language-servers
    ## For integration with Emacs and Neovim language support
    tree-sitter
    # luajitPackages.luarocks

    ## Help
    ### tldr in rust
    tealdeer
    ## Prettier formatter multi-language
    nodePackages.prettier
    ## English
    ### Prose English linter for Markdown, Org
    proselint
    ### Grammer checker (US English)
    ltex-ls-plus

    ## Go lang
    go
    ### Go LSP
    gopls
    ### Go coding deps, See Doom Emacs Go module README
    ### godoc, goimports
    gotools
    ## Lua
    lua-language-server
    ## Markdown LSP
    marksman
    ## Nix
    ### Nix LSP
    nil
    ### Nix formatter
    nixpkgs-fmt
    ## Node
    ## alias for LTS version configured in all-packages per https://nixos.wiki/wiki/Node.js
    nodejs
    ## PlantUML
    plantuml
    graphviz
    ## pandoc
    ### Used in document conversion, Emacs exports
    pandoc

    ## Python
    ### Python LSP (or basedpyright later)
    pyright
    ### Python formatter
    black
    ### Python linter
    ruff

    ## Rust
    ### Debugger for Rust / C / C++
    lldb
    ### Key components from rustup https://rust-lang.github.io/rustup-components-history/
    ### Defer to rustup based installs for:
    ### cargo
    ### rustc
    ### clippy
    #### Rust watch source like tests
    ### cargo-watch
    #### Rust LSP
    ### rust-analyzer
    #### Rust formatter
    ### rustfmt

    ## Shell/Bash
    ### Bash LSP
    bash-language-server
    #### Shellcheck used by Emacs Flymake for shell scripts by default - Emacs 29.1
    shellcheck
    ### Shell formatter
    shfmt

    ## Terraform
    ### Terraform LSP
    terraform-ls
    ### Terraform Linter
    tflint

    ## toml
    taplo

    ## yaml
    yaml-language-server

    # Infrastructure as Code
    distrobox

    # Artificial Intelligence
    ollama

    # Cloud
    azure-cli

    # Windows compatibility
    # Wine with 32 and 64 bit per https://nixos.wiki/wiki/Wine
    wineWowPackages.stable
    winetricks

    # Security
    sops

    # ------------------------------------------------------------
    # Graphical Applications Tested with Nix by not managed by Nix

    # Graphics and Diagrams
    # gimp
    # inkscape
    # drawio

    # Files
    # fsearch
    # Security
    # keepassxc
    # Email
    # thunderbird
    # RSS
    # fluent-reader
    # Video
    # freetube

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Allow fontconfig to discover fonts and configurations installed through home.packages and nix-env
  # per https://github.com/nix-community/home-manager/issues/605
  fonts.fontconfig = { enable = true; };

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/justin/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Emacs program and
  # vterm package setup https://docs.doomemacs.org/latest/modules/term/vterm/
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  # Enable setting profile and session variables for system
  # and update of user environment cache of programs
  # Allow programs to show in desktop environment
  # per https://github.com/nix-community/home-manager/issues/1439
  targets.genericLinux.enable = true;
  xdg.mime.enable = true;
}
