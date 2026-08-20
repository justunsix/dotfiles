{ config, pkgs, ... }:

{

  imports = [
    # User, fonts config, home-manager, allow unfree packages
    ./modules/base.nix
    ./modules/graphical-programs.nix
    # GNOME Desktop and its settings, System services
    ./modules/desktop.nix
    # All other packages like for terminal, fonts, system management, computer programming, devops
    ./modules/tools.nix
  ];

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

  # # You can also create simple shell scripts directly inside your
  # # configuration. For example, this adds a command 'my-hello' to your
  # # environment:
  # (writeShellScriptBin "my-hello" ''
  #   echo "Hello, ${config.home.username}!"
  # '')

}
