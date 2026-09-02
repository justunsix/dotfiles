{
  config,
  lib,
  pkgs,
  ...
}:

{

  # Desktop Configuration overlay for:
  # - GNOME File Manager
  # - Enable media inforamtion in Nautilus: https://wiki.nixos.org/wiki/Nautilus
  nixpkgs.overlays = [
    (final: prev: {
      nautilus = prev.nautilus.overrideAttrs (nprev: {
        buildInputs =
          nprev.buildInputs
          ++ (with pkgs.gst_all_1; [
            gst-plugins-good
            gst-plugins-bad
          ]);
      });
    })
  ];
}
