{ config, pkgs, ... }:

{

  imports = [
    # User, fonts config, home-manager, allow unfree packages
    ../modules/home-manager/base.nix
    ../modules/home-manager/graphical-programs.nix
    # All other packages like for terminal, fonts, system management, computer programming, devops
    ../modules/home-manager/tools.nix
  ];

  # Settings that make Home Manager work better on GNU/Linux distributions other than NixOS
  targets.genericLinux.enable = true;

  # GPU use on non NixOS Linux systems, like for graphical programs
  # Set using two options:
  # 1. home-manager https://nix-community.github.io/home-manager/usage/gpu-non-nixos.html
  # 2. NixGL https://github.com/nix-community/nixGL
  targets.genericLinux.gpu.enable = true;

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

}
