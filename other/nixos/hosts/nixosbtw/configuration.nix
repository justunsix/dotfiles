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

let
  pkgs-unstable = import inputs.nixpkgs-unstable {
    # Use same platform as host
    system = pkgs.system;
    config.allowUnfree = true;
  };
in
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
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
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Toronto";

  # Setting RTC time standard to localtime, compatible with Windows in its default configuration
  # https://nixos.wiki/wiki/Dual_Booting_NixOS_and_Windows
  time.hardwareClockInLocalTime = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_CA.UTF-8";

  # Optionally (BEWARE: requires a different format with the added /UTF-8)
  i18n.extraLocales = [ "en_US.UTF-8/UTF-8" ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  environment.gnome.excludePackages = (
    with pkgs;
    [
      # atomix # puzzle game
      # cheese # webcam tool
      # epiphany # web browser
      # decibels # audio player
      # evince # document viewer
      # geary # email reader
      # gedit # text editor
      # gnome-characters
      gnome-connections # remote desktop
      # gnome-music
      # gnome-photos
      # gnome-terminal
      # gnome-tour
      # showtime # video player
    ]
  );

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

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
      keepassxc

    ];
  };

  # Install firefox
  programs.firefox.enable = true;

  # Run unpatched dynamic binaries on NixOS
  # like uv https://wiki.nixos.org/wiki/Python_quickstart_using_uv
  programs.nix-ld.enable = true;

  # Allow removable media management
  services.udisks2.enable = true;

  # Steam gaming
  # https://nixos.wiki/wiki/Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Graphics Configuration
  # AMD https://wiki.nixos.org/wiki/AMD_GPU
  # NVIDIA https://nixos.wiki/wiki/Nvidia

  # Enable OpenGL
  hardware.graphics.enable = true;
  # Use the NVidia open source kernel module (not to be confused with the
  # independent third-party "nouveau" open source driver).
  # Support is limited to the Turing and later architectures. Full list of
  # supported GPUs is at:
  # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
  # Only available from driver 515.43.04+
  hardware.nvidia.open = true;
  # Enable AMD ROCM or NVIDIA CUDA support for nixpkgs that have it like pkgs.btop
  # nixpkgs.config.rocmSupport = true;
  # CUDA Setup https://wiki.nixos.org/wiki/CUDA
  # Enable CUDA Cache - requires one nixos-rebuild before it's available
  nix.settings = {
    substituters = [
      "https://cache.nixos-cuda.org"
    ];
    trusted-public-keys = [
      "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
    ];
  };
  # nixpkgs.config.cudaSupport = true;

  ## Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {

    # Wayland requires kernel mode setting (KMS) to be enabled
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    # Enable this if you have graphical corruption issues or application crashes after waking
    # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead
    # of just the bare essentials.
    powerManagement.enable = false;

    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    # package = config.boot.kernelPackages.nvidiaPackages.stable;
    #
    # LTSB supported until Aug 2028
    # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/os-specific/linux/nvidia-x11/default.nix
    # Try due to error, unknown if caused by 595 and other series drivers:
    # https://forums.developer.nvidia.com/t/nvidia-drm-error-flip-event-timeout-on-head-0-system-hangs-during-boot/374743
    # package = config.boot.kernelPackages.nvidiaPackages.legacy_580;

    # Temporary workaround for build failure 2026-08-18
    # https://github.com/NixOS/nixpkgs/issues/554125
    package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
      version = "595.91.07";
      sha256_64bit = "sha256-yiPIjdJLB6GRZE4eEc+3vN11NzBXSa9A+YABiwleYxM=";
      sha256_aarch64 = "sha256-fqkN7ONFXtTeXyu2mQxorrk362Epxq3bz88hhKYQzwQ=";
      openSha256 = "sha256-OB8Epd+qn/WywxsPiFpxEOAzlJqb6I1SyRoV3a8l71k=";
      settingsSha256 = "sha256-QzT8Cw1luuZGP9DUje3HN/0ngiayqHURj+bqPsxlJ5w=";
      persistencedSha256 = "sha256-3JQBaNmkwxvCXv9q8aHKas6VZM/JjLsuilC2t7ET0u0=";
    };
  };

  # Firmware updates
  # https://wiki.archlinux.org/title/Fwupd
  services.fwupd.enable = true;

  # Enable flakes https://nix.dev/concepts/flakes.html
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default
    git
    # exfat disk utilities like fsck.exfat
    exfatprogs

    # btop on NVIDIA
    btop-cuda

    # Declare packages in in nixpkgs-unstable with pkgs.unstable prefix
    # pkgs-unstable.firefox
  ];

  # VirtualBox - https://wiki.nixos.org/wiki/VirtualBox
  # virtualisation = {
  #   virtualbox.host.enable = true;
  #   virtualbox.host.enableExtensionPack = true;
  #   virtualbox.guest.enable = true;
  #   virtualbox.guest.dragAndDrop = true;
  # };
  # # Users with access to VirtualBox
  # users.extraGroups.vboxusers.members = [ "justin" ];

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

  # Key binding service
  # Autoload from https://github.com/thursdaddy/nixos-config/blob/f21380b188bd3941b32656e832c65111c437f463/modules/desktop/input-remapper.nix
  services.input-remapper = {
    enable = true;
    serviceWantedBy = [ "multi-user.target" ];
  };

  systemd.user.services.input-remapper-autoload = {
    description = "Run input-remapper-control autoload command";
    documentation = [ "https://github.com/sezanzeb/input-remapper" ];
    after = [ "graphical-session.target" ];
    bindsTo = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.input-remapper}/bin/input-remapper-control --command autoload";
      Restart = "on-failure";
      RestartSec = "5s";
      KillMode = "mixed";
    };
  };

  # Portmaster - Package - Secure DNS, firewall, network monitoring
  # How to use at https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/networking/portmaster.md
  services.portmaster = {
    enable = true;
    package = pkgs-unstable.portmaster;
    # Set only when unrestricted browser or debugging access to http://127.0.0.1:817 is required
    settings.devmode = true;
  };
  # Portmaster - Startup - do not autostart, start manually due to interference with other progams at startup
  systemd.services.portmaster.wantedBy = lib.mkForce [ ];
  # It's default is start up before netowrking:
  # wantedBy = [ "multi-user.target" ];
  # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/nixos/modules/services/networking/portmaster.nix
  #
  # Portmaster - Nix Manual - Portmaster is in nixpkgs-unstable and ships manual docs where
  # chapter identifiers aren't registered in nixpkgs-stable's redirects.json as of 2026-08-08
  # Skip that check suggested at https://github.com/NixOS/nixpkgs/issues/412451
  documentation.nixos.checkRedirects = false;

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
