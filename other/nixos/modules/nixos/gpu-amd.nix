{ pkgs, ... }:
{

  # Graphics Configuration
  # https://wiki.nixos.org/wiki/AMD_GPU

  # Enable AMD ROCM support for nixpkgs that have it like pkgs.btop
  environment.systemPackages = with pkgs; [
    btop-rocm
  ];
  
  # Enabling ROCm & HIP For Packages
  nixpkgs.config.rocmSupport = true;

}