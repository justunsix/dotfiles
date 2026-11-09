{
  pkgs-stable,
  ...
}:

{
  # ------------------------------------------------------------
  # Packages pinned to the stable nixpkgs input in flake
  home.packages = with pkgs-stable; [

    # System Management
    topgrade

    # Shell
    nushell
    nushellPlugins.polars

    # File Management
    gfold

    # ------------------------------------------------------------
    # Graphical Applications with Nix

    # Books, Library, Knowledge
    calibre
    anki

    # Information Management
    ## Library and Bibliography management
    zotero

    ## Video upscale
    video2x

  ];

}
