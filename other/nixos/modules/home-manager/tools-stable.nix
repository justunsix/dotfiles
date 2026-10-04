{
  pkgs-stable,
  ...
}:

{
  # ------------------------------------------------------------
  # Packages pinned to the stable nixpkgs input in flake
  home.packages = with pkgs-stable; [

    # ------------------------------------------------------------
    # Graphical Applications with Nix

    # Information Management
    ## Library and Bibliography management
    zotero

  ];

}
