{ pkgs, ... }:
{

  # VirtualBox - https://wiki.nixos.org/wiki/VirtualBox
  virtualisation = {
    virtualbox.host.enable = true;
    virtualbox.host.enableExtensionPack = true;
    virtualbox.guest.enable = true;
    virtualbox.guest.dragAndDrop = true;
  };
  # # Users with access to VirtualBox
  users.extraGroups.vboxusers.members = [ "justin" ];

}