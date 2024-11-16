# Broot - File manager, directory navigator
source "~/AppData/Roaming/dystroy/broot/config/launcher/nushell/br"

# Atuin - Shell history
source ~/.local/share/atuin/init.nu

# Yazi File Manager to open files on Windows https://yazi-rs.github.io/docs/installation 
$env.YAZI_FILE_ONE = ($nu.home-path | path join "scoop/apps/git/current/usr/bin/file.exe")
