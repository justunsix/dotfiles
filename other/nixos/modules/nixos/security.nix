{ pkgs, ... }:
{
  # Security services like malware scanning and anti-virus

  environment.systemPackages = (
    with pkgs;
    [
      clamav
    ]
  );

  services.clamav.daemon.enable = true;
  services.clamav.updater.enable = true;

}
