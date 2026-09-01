{ pkgs, ... }:
{
  # Desktop Configuration with:
  # - GNOME Desktop Environment
  # - Display Manager (GDM)
  # - Printing
  # - Keyboard setup
  # - Graphical environment settings
  # - Web browser

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  environment.gnome.excludePackages = (
    with pkgs;
    [
      # atomix # puzzle game
      # cheese # webcam tool
      # epiphany # web browser
      # decibels # audio player
      # evince # document viewer
      # geary # email reader
      # gedit # text editor
      # gnome-characters
      gnome-connections # remote desktop
      # gnome-music
      # gnome-photos
      # gnome-terminal
      # gnome-tour
      # showtime # video player
    ]
  );

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;
  
  # Install firefox
  programs.firefox.enable = true;

  # Nautilus media information
  # https://wiki.nixos.org/wiki/Nautilus
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