{
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [

    # ------------------------------------------------------------
    # Media and Web Downloads

    ## yt-dlp and metadata
    ### yt-dlp requirements and optionals
    yt-dlp
    #### Video processing, yazi optional dependency for video thumbnails
    ffmpeg_7-full
    #### YouTube Support
    python314Packages.yt-dlp-ejs
    ##### YT Support, JS Engine, declared below using deno
    ##### Root Certificates
    python314Packages.certifi
    ##### Encoding
    python314Packages.brotli
    ##### Downloading
    python314Packages.websockets
    #### HTTP library
    python314Packages.requests
    #### Impersonation
    python314Packages.curl-cffi
    #### Metadata
    ##### thumbnail
    python314Packages.mutagen
    atomicparsley
    #### xattr metadata
    python314Packages.xattr
    #### Other
    ##### Decryption of streams
    python314Packages.pycryptodome
    #### Downloader
    aria2

    # Torrents
    transmission_4-gtk
    transmission-remote-gtk

  ];

}
