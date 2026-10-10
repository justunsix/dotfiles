{ pkgs, ... }:
{

  # Gaming with
  # - Steam
  # - Lutris

  environment.systemPackages = with pkgs; [
    lutris
  ];

  # Steam gaming
  # https://nixos.wiki/wiki/Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
  };

}
