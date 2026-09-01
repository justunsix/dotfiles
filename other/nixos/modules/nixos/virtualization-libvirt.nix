{ pkgs, ... }:
{

  environment.systemPackages = with pkgs; [
    gnome-boxes
  ];

# Virtualization, Virtual Machines
  # https://wiki.nixos.org/wiki/Libvirt
  # https://nixos.wiki/wiki/Virt-manager
  virtualisation.libvirtd.enable = true;
  # Enable TPM emulation (optional)
  # install pkgs.swtpm system-wide for use in virt-manager (optional)
  virtualisation.libvirtd.qemu = {
    swtpm.enable = true;
  };
  # Enable USB redirection (optional)
  virtualisation.spiceUSBRedirection.enable = true;
  programs.virt-manager.enable = true;
  users.groups.libvirtd.members = [ "justin" ];

}