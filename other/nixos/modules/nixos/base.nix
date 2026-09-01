{ pkgs, ... }:
{

  # Base system configuration with:
  # - Networking
  # - Time zone and localization
  # - Nix, nixpkgs settings
  # - Firmware updates
  # - System packages

  # Enable networking
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Toronto";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_CA.UTF-8";

  # Optionally (BEWARE: requires a different format with the added /UTF-8)
  i18n.extraLocales = [ "en_US.UTF-8/UTF-8" ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      # inputs.self.overlays.additions
      # inputs.self.overlays.modifications
      # inputs.self.overlays.unstable-packages

      # You can also add overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
    };
  };

  nix = {
    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Opinionated: disable global registry
      # flake-registry = "";
    };
  };

  # Firmware updates
  # https://wiki.archlinux.org/title/Fwupd
  services.fwupd.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default
    git
  ];

  # Run unpatched dynamic binaries on NixOS
  # like uv https://wiki.nixos.org/wiki/Python_quickstart_using_uv
  programs.nix-ld.enable = true;
  # https://nix.dev/guides/faq#how-to-run-non-nix-executables
  # Create a library path that only applies to unpackaged programs by using nix-ld. Add this to your configuration.nix:
  #programs.nix-ld.libraries = with pkgs; [
    # Add any missing dynamic libraries for unpackaged programs
    # here, NOT in environment.systemPackages
  #];



}