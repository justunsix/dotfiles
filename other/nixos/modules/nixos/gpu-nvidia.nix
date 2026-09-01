{ pkgs, ... }:
{

  # Graphics Configuration
  # NVIDIA https://nixos.wiki/wiki/Nvidia

  # Enable NVIDIA CUDA support for nixpkgs that have it like pkgs.btop
  environment.systemPackages = with pkgs; [
    btop-cuda
  ];

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
}