{ config, pkgs, ... }:

{

  imports = [
    ./modules/graphical-programs.nix
  ];

  home.username = "justin";
  home.homeDirectory = "/home/justin";
  home.stateVersion = "26.05";

  # Pick up fonts
  fonts.fontconfig = {
    enable = true;
  };

  # home.file = {
  # # Building this configuration will create a copy of 'dotfiles/screenrc' in
  # # the Nix store. Activating the configuration will then make '~/.screenrc' a
  # # symlink to the Nix store copy.
  # ".screenrc".source = dotfiles/screenrc;

  # # You can also set the file content immediately.
  # ".gradle/gradle.properties".text = ''
  #   org.gradle.console=verbose
  #   org.gradle.daemon.idletimeout=3600000
  #   '';
  # };

  home.packages = with pkgs; [

    # ------------------------------------------------------------
    # Main Packages

    # System
    curl
    ## Wayland clipboard management
    wl-clipboard

    # Shell
    nushell
    nushellPlugins.polars
    ## Nushell formatter
    nufmt
    starship
    bat
    atuin
    carapace
    ### fzf optional dependency for yazi file search and editors
    fzf
    television
    ## rg optional for nvim and emacs frameworks
    ### rg optional dependency for yazi content search
    ripgrep

    # Fonts
    iosevka
    ## Doom Emacs
    ### Default font
    jetbrains-mono
    ## Starship prompt, eza nerd font
    nerd-fonts.fira-code
    source-code-pro
    nerd-fonts.jetbrains-mono
    # File Management
    p7zip
    gfold
    duf
    stow
    yazi
    kondo
    ## Git
    lazygit
    worktrunk
    ### git diff
    delta

    ## Media and Web, yt-dlp, metadata
    ### yt-dlp requirements and optionals
    yt-dlp
    #### Video processing, yazi optional dependency for video thumbnails
    ffmpeg_7-full
    #### YouTube Support
    python314Packages.yt-dlp-ejs
    ##### YT Support, JS Engine, declared below using deno
    ##### Root Certificates
    python314Packages.certifi
    ##### Encoding
    python314Packages.brotli
    ##### Downloading
    python314Packages.websockets
    #### HTTP library
    python314Packages.requests
    #### Impersonation
    python314Packages.curl-cffi
    #### Metadata
    ##### thumbnail
    python314Packages.mutagen
    atomicparsley
    #### xattr metadata
    python314Packages.xattr
    #### Other
    ##### Decryption of streams
    python314Packages.pycryptodome
    #### Downloader
    aria2

    # Web
    lynx

    # Productivity and Tasks
    todo-txt-cli
    gnumake
    # System and Monitors
    btop
    espanso
    ## Clean files
    bleachbit

    # Terminal
    zellij
    mprocs
    # Video, Music
    mpv
    ## Media, Sound, Volume control
    pavucontrol
    ## Former rust cargo installs
    dust
    eza
    topgrade
    ### zoxide optional dependency for yazi directory navigation
    zoxide
    # DevOps
    ## Containers
    podman
    ## Tools, Env
    asdf-vm
    mise
    ### Doom Emacs requirements
    emacs
    #### fd also yazi dependency for file search
    fd
    ## Other Editors
    neovim
    helix

    # Languages
    ## Language Servers installs, LSP, Linters
    ## https://github.com/helix-editor/helix/wiki/How-to-install-the-default-language-servers
    ## For integration with Emacs and Neovim language support
    tree-sitter
    ### Neovim Mason dependencies
    unzip
    ## C
    gcc
    # luajitPackages.luarocks

    ## Help
    ### tldr in rust
    tealdeer
    ## Prettier formatter multi-language
    prettier
    ## English, Other Spoken Languages
    ### Prose English linter, spell check for Markdown, Org
    ### Used by Doom Emacs
    proselint
    (aspellWithDicts (
      dicts: with dicts; [
        en
        en-computers
        en-science
      ]
    ))
    ### Grammer checker (US English)
    ### Language Tool
    #### Java version used by Emacs
    languagetool
    #### LSP used by Helix
    ltex-ls-plus

    ## JSON
    jq
    ### LSP used by Helix
    vscode-langservers-extracted

    ## Lua
    lua-language-server
    ## Markdown LSP
    marksman

    ## Nix
    ### Nix LSP
    nil
    ### Nix formatter
    nixfmt

    ## Java
    ### https://nixos.wiki/wiki/Java
    jdk

    ## JavaScript
    ### alias for LTS version configured in all-packages per https://nixos.wiki/wiki/Node.js
    nodejs
    ### Faster, security permissions JS runtime
    deno

    ### Package and version manager
    pnpm

    ## PlantUML
    plantuml
    graphviz

    ## pandoc
    ### Used in document conversion, Emacs exports
    pandoc

    ## Go lang
    go
    ### Go LSP
    gopls
    ### Go coding deps, See Doom Emacs Go module README
    ### godoc, goimports
    # gotools

    ## Python
    python3
    uv
    ### Python LSP
    pyright
    ty
    ### Python formatter
    black
    ### Python linter
    ruff

    ## Rust
    ### Debugger for Rust / C / C++
    lldb
    ### Key components from rustup https://rust-lang.github.io/rustup-components-history/
    ### Defer to rustup based installs for:
    ### rustup after, run rustup default stable && rustup component add rust-analyzer
    #### Rust compiler, package and task management
    rustc
    cargo
    #### Rust Linter
    clippy
    #### Rust LSP
    rust-analyzer
    #### Rust formatter
    rustfmt

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

    ## xml
    ### xml LSP
    lemminx
    ### xmllint - linter, format
    libxml2

    # Data Science, Artificial Intelligence
    opencode

    # Security
    proton-vpn-cli
    ## Files
    gnupg
    sops
    veracrypt
    ## Anti-virus
    clamav

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

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # GNOME Settings
  dconf = {
    settings = {
      # Dark mode for GTK apps
      "org/gnome/desktop/interface".color-scheme = "prefer-dark";
    };
  };

  services.udiskie = {
    enable = true;
    settings = {
      # workaround for
      # https://github.com/nix-community/home-manager/issues/632
      program_options = {
        # replace with your favorite file manager
        file_manager = "${pkgs.nautilus}/bin/nautilus";
      };
    };
  };

}
