{
  config,
  lib,
  pkgs,
  ...
}:

{

  services.guix = {
    enable = true;
    package = pkgs.guix;
  };

}
