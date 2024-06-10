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

    # System
    procs
    htop
    redshift

    # Shell
    fish
    nushell
    starship
    bat
    atuin
    carapace
    rofi
    rename
    ripgrep
    dogdns
    # Fonts
    iosevka
    source-code-pro
    # Even though installed by apt, require for program like exa
    fira-code
    # File Management
    vcstool
    gfold
    img2pdf
    duf
    stow
    yazi
    lazygit
    ## bottom aka btm
    bottom
    kondo
    broot
    ranger
    # Terminal
    tmux
    mprocs
    ## sd
    ## Former rust cargo installs
    du-dust
    ### Remove unmaintained - exa, replaced by eza
    eza
    monolith
    topgrade
    zoxide
    # DevOps
    emacs
    ### Doom Emacs requirements
    fd
    ## Other Editors
    neovim
    helix
    ### Astronvim requirements
    tree-sitter
    ## Node
    ### LTS as of 2023-04
    nodejs_20
    ## PlantUML
    plantuml
    ## Document conversion, Emacs exports
    pandoc
    ## Python
    conda
    ## Rust
    ### https://github.com/rust-lang/rustlings/blob/main/flake.nix
    cargo
    rustc
    rust-analyzer
    clippy
    ## Language Servers, LSP
    ## https://github.com/helix-editor/helix/wiki/How-to-install-the-default-language-servers
    ### Markdown
    marksman
    ## Infrastructure as Code
    ### vagrant

    # ------------------------------------------------------------
    # Graphical Applications Tested with Nix by not managed by Nix

    # Productivity and Tasks
    # super-productivity
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

  # Pick up fonts
  # per https://github.com/nix-community/home-manager/issues/605
  fonts.fontconfig = {
   enable = true;
  };

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

  # Enable setting profile and session variables for system
  # and update of user environment cache of programs
  # Allow programs to show in desktop environment
  # per https://github.com/nix-community/home-manager/issues/1439
  targets.genericLinux.enable = true;
  xdg.mime.enable = true;
}
