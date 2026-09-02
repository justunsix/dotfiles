# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../../modules/nixos/base.nix
    ../../modules/nixos/desktop-gnome.nix
    ../../modules/nixos/desktop-gnome-nautilus-overlay.nix
    ../../modules/nixos/audio.nix
    ../../modules/nixos/gaming.nix
    ../../modules/nixos/key-mapper-input-remapper.nix
    ../../modules/nixos/networking-portmaster.nix
    ../../modules/nixos/gpu-amd.nix
    ../../modules/nixos/virtualization-libvirt.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "nixosmis"; # Define your hostname.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users."justin" = {
    isNormalUser = true;
    description = "justin";
    extraGroups = [
      "networkmanager"
      "wheel"
      # for work with rdev crate https://crates.io/crates/rdev
      "input"
    ];
    packages = with pkgs; [
      #  thunderbird
    ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Secondary Storage
  # https://wiki.nixos.org/wiki/Full_Disk_Encryption#Unlocking_secondary_drives
  environment.etc.crypttab.text = ''
    briar UUID=5a5fb28d-9ab8-4bb0-b73b-e4cd16bd752a /root/mykeyfile.key luks,nofail
  '';

  # nofail plus a short device timeout
  fileSystems."/run/media/justin/Briar" = {
    device = "/dev/mapper/briar";
    fsType = "ext4";
    options = [
      "nofail"
      "x-systemd.device-timeout=10s"
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?

}
