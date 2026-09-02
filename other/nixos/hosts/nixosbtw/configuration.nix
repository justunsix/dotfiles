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
    ../../modules/nixos/gpu-nvidia.nix
  ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sdb";
  boot.loader.grub.useOSProber = true;
  # Workaround to prevent /boot from filling up
  # https://github.com/NixOS/nixpkgs/issues/23926
  boot.loader.systemd-boot.configurationLimit = 3;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "nixosbtw"; # Define your hostname.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Setting RTC time standard to localtime, compatible with Windows in its default configuration
  # https://nixos.wiki/wiki/Dual_Booting_NixOS_and_Windows
  time.hardwareClockInLocalTime = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users."justin" = {
    isNormalUser = true;
    description = "justin";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    packages = with pkgs; [
      # keepassxc

    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # exfat disk utilities like fsck.exfat
    exfatprogs
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #  enable = true;
  #  enableSSHSupport = true;
  #};

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?

}
