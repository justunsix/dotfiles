{ pkgs, inputs, ... }:
let
  pkgs-unstable = import inputs.nixpkgs-unstable {
    # Use same platform as host
    system = pkgs.system;
    config.allowUnfree = true;
  };
in
{
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

}