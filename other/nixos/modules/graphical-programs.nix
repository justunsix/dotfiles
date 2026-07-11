{
  pkgs,
  ...
}:

{
  # Install Neovim and dependencies
  home.packages = with pkgs; [

    # ------------------------------------------------------------
    # Graphical Applications Tested with Nix

    # Browser
    firefox

    # Clipboard manager
    copyq

    # Information Management
    ## Library and Bibliography management
    zotero

    # Terminals
    # wezterm
    alacritty

    # Graphics and Diagrams
    # gimp
    # inkscape
    # drawio

    # Files
    # fsearch
    # Security
    keepassxc
    # Email
    # thunderbird
    # RSS
    # fluent-reader
    # Video
    freetube

    # Video
    vlc
  ];
}
