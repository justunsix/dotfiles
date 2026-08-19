{
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [

    # ------------------------------------------------------------
    # Graphical Applications with Nix

    # Browser
    ungoogled-chromium
    firefox

    # Books, Library, Knowledge
    calibre
    anki

    # Clipboard manager
    copyq

    # File Management
    kdePackages.dolphin
    ## KDE theme configuration
    # kdePackages.qt6ct

    # Information Management
    ## Library and Bibliography management
    zotero

    # Terminals
    # wezterm
    alacritty
    ghostty

    # Graphics and Diagrams
    gimp
    inkscape
    # drawio

    # Files
    fsearch
    peazip

    # Security
    keepassxc
    ## Certificate manager and GUI for OpenPGP, GNUPG and CMS cryptography
    ### Remove for now due to inference with GPUPG sockets
    ### kdePackages.kleopatra

    # Video
    freetube
    vlc
    kdePackages.kdenlive

    # Productivity
    libreoffice
    ## Email
    ## thunderbird

  ];

}
