{
  pkgs,
  ...
}:

{
  # Install Neovim and dependencies
  home.packages = with pkgs; [

    # ------------------------------------------------------------
    # Graphical Applications with Nix

    # Browser
    firefox
    ungoogled-chromium

    # Books, Library, Knowledge
    calibre
    anki

    # Clipboard manager
    copyq

    # Information Management
    ## Library and Bibliography management
    zotero

    # Terminals
    # wezterm
    alacritty

    # Graphics and Diagrams
    gimp
    inkscape
    # drawio

    # Files
    fsearch
    peazip

    # Security
    keepassxc

    # Video
    freetube
    vlc
    kdePackages.kdenlive

    # Productivity
    libreoffice
    keepassxc
    ## Email
    ## thunderbird

  ];

  # Services
  services.copyq.enable = true;
}
