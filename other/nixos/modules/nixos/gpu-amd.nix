{ pkgs, ... }:
{

  # Graphics Configuration

  # NVIDIA https://nixos.wiki/wiki/Nvidia

  # Enable NVIDIA CUDA support for nixpkgs that have it like pkgs.btop
  environment.systemPackages = with pkgs; [
    btop-rocm
  ];
  
  # Enabling ROCm & HIP For Packages
  nixpkgs.config.rocmSupport = true;

}