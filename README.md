# JT's Dotfiles

Configuration and customization files to personalize Linux, Windows, and macOS.

This repository contains my personal dotfiles like configurations for software described below.

## Repository Structure

List of folders and dotfiles in them:

+ `.config/` configuration files for the below mentioned programs<br/>
++ `alacritty` - [alacritty](https://alacritty.org) terminal emulator
++ `broot` - [Broot](https://dystroy.org/broot/) file and directory navigator<br/>
++ `Code/User/` - [Visual Studio Code](https://code.visualstudio.com/) editor<br/>
++ `cool-retro-term/` - [Cool Retro Term](https://github.com/Swordfish90/cool-retro-term) Terminal Emulator<br/>
++ `copyq/` - [copyq](https://hluk.github.io/CopyQ/) Clipboard Manager<br/>
++ `doom/` - [Doom Emacs Framework](https://github.com/doomemacs/doomemacs)<br/>
++ `dunst/` - [dunst](https://dunst-project.org/) Notification daemon, for use with i3<br/>
++ `emacs/` - [Emacs](https://www.gnu.org/software/emacs/) editor<br/>
++ `fish/` - [Fish](https://fishshell.com/) shell<br/>
++ `fontconfig/conf.d/` - Fonts managed by [Nix package manager](https://nixos.org/)<br/>
++ `helix` - [Helix](https://helix-editor.com/) editor<br/>
++ `home-manager` - [Home Manager using nix](https://github.com/nix-community/home-manager) for software packages<br/>
++ `i3/` - [i3 Window Manager (WM)](https://i3wm.org/)<br/>
++ `i3status/` - [i3status](https://i3wm.org/docs/i3status.html) Status bar for use with i3<br/>
++ `input-remapper/` - [input-remapper](https://github.com/sezanzeb/input-remapper) to change input behaviour like key remapping<br/>
++ `lazyvim` - [Lazyvim](https://www.lazyvim.org/) Neovim setup
++ `mprocs/` - [mprocs](https://github.com/pvolok/mprocs) to manage parallel and commonly executed processes<br/>
++ `mpv/` - [mpv](https://mpv.io/) media player<br/>
++ `nushell/` - [nushell](https://www.nushell.sh/) cross platform shell<br/>
++ `nvim/` - [neovim](https://neovim.io/) editor<br/>
++ `rofi/` - [rofi](https://github.com/davatorium/rofi) Application launcher, Window switcher for use with i3 and in GNOME<br/>
++ `tmux` - [tmux](https://github.com/tmux/tmux/wiki) Terminal Multiplexer<br/>
++ `todotxt-cli/` - [todotxt](https://github.com/todotxt/todo.txt-cli) Task manager on command line<br/>
++ `topgrade/` - [topgrade](https://github.com/topgrade-rs/topgrade) Updater for things like operating systems, software, packages, and others<br/>
++ `vlc/` - [vlc](https://www.videolan.org/vlc/) media player<br/>
++ `wezterm/` - [WezTerm](https://wezfurlong.org/wezterm/index.html) Cross platform terminal emulator and multiplexer<br/>
++ `yazi/` - [Yazi](https://yazi-rs.github.io/) file manager<br/>
++ `dolphinrc` - [Dolphin File Manager](https://apps.kde.org/dolphin/) settings<br/>
++ `kdeglobals` - [KDE](https://kde.org/) KDE Desktop Environment appearance settings<br/>
++ `gfold.toml` - [gfold](https://github.com/nickgerace/gfold) Track git repositories<br/>
++ `starship.toml` - [Starship](https://starship.rs/) Cross-shell prompt<br/>[Starship](https://starship.rs/) Cross-shell prompt<br/>
+ `.fonts/` - Open source fonts<br/>
+ `.local/share/applications/` - [Desktop entries](https://wiki.archlinux.org/title/desktop_entries)<br/>
+ `Templates/` - New file templates for [GNOME File manager](https://wiki.gnome.org/action/show/Apps/Files?action=show&redirect=Apps%2FNautilus) also known as Nautilus<br/>
+ `usr/bin/` - Linux scripts<br/>
+ `usr/bin-windows/` - Windows scripts<br/>
+ `other/` - Other configuration files not normally stored in user home directory<br/>
++ `PowerShell/` - [PowerShell](https://learn.microsoft.com/en-us/powershell/scripting/overview?view=powershell) shell<br/>
++ `WindowsPowerShell/` - [Windows PowerShell](https://learn.microsoft.com/en-us/powershell/scripting/windows-powershell/starting-windows-powershell) 5.1 shell<br/>
+ `.bash...` - [Bash](https://www.gnu.org/software/bash/) shell<br/>
+ `.gitconfig` - [Git](https://git-scm.com/) distributed version control configuration file<br/>
+ `.inputrc` - [GNU Readline](https://tiswww.cwru.edu/php/chet/readline/rltop.html) for command line editing<br/>
+ `.Xresources` , `.xinitrc` - [X Window System](https://www.x.org/wiki/) configuration files<br/>

## Programs and Software Configured

### Operating System and Environment

- Operating systems/distributions:
  - [GNU/Linux Ubuntu using GNOME](https://ubuntu.com/desktop)
  - [GNU/Linux Fedora using i3 spin](https://spins.fedoraproject.org/en/i3/)
  - [Microsoft Windows](https://www.microsoft.com/en-ca/windows)
- Desktop environment: [GNOME](https://www.gnome.org/)
- Window manager (WM): [i3 tiling WM](https://i3wm.org/)
  - Status bar: [i3status](https://i3wm.org/docs/i3status.html)
- Shell:
  - [Bash](https://www.gnu.org/software/bash/) 
    - [GNU Readline](https://tiswww.cwru.edu/php/chet/readline/rltop.html) library
  - [Fish](https://fishshell.com/)
  - [Nushell](https://www.nushell.sh/) cross platform shell
  - [PowerShell](https://learn.microsoft.com/en-us/powershell/scripting/overview?view=powershell)
  - Prompt: [Starship](https://starship.rs/)

### Programs

- Version control: 
  - [Git](https://git-scm.com/)
  - [gfold](https://github.com/nickgerace/gfold) repositories tracker
- Editor:
  - [Emacs](https://www.gnu.org/software/emacs/) with [Doom Framework](https://github.com/doomemacs/doomemacs)
  - [Visual Studio Code](https://code.visualstudio.com/)
  - [neovim](https://neovim.io/)
  - [Helix](https://helix-editor.com/)
- Video player: [mpv](https://mpv.io/) and [vlc](https://www.videolan.org/vlc/)

- Window switcher/application launcher: [rofi](https://github.com/davatorium/rofi)
- Clipboard manager: [copyq](https://hluk.github.io/CopyQ/)
- Terminal:
  - [Cool Retro Term](https://github.com/Swordfish90/cool-retro-term)
  - [WezTerm](https://wezfurlong.org/wezterm/index.html) which includes multiplexer 
  - Multiplexer: [tmux](https://github.com/tmux/tmux/wiki)
- Notification daemon: [dunst](https://dunst-project.org/)
- Personal Task management: [todotxt](https://github.com/todotxt/todo.txt-cli)
- Parallel commands manager: [mprocs](https://github.com/pvolok/mprocs)
- Updates for software: [topgrade](https://github.com/topgrade-rs/topgrade)
- Keyboard mapper: [input-remapper](https://github.com/sezanzeb/input-remapper)
- Package Management: [Home Manager using nix](https://github.com/nix-community/home-manager)
- File Explorer, Directory Navigator: [Broot](https://dystroy.org/broot/), [Dolphin File Manager](https://apps.kde.org/dolphin/)

### Operating System Compatibility

- Most software listed above runs cross-platform on Linux, Windows, and macOS and can use the same configuration files (dotfiles). The dotfiles are regularly tested on Linux and Windows using latest stable versions of software.
- For Linux / Unix only software, it can be run on Windows using Windows Subsystem for Linux (WSL), Cygwin, and/or MSYS on Windows (examples: GNOME, i3, rofi, fish, tmux, dunst, home manager).

## How Dotfiles are Managed

There are many ways to manage your dotfiles. I use an [Ansible playbook](https://github.com/justunsix/dotfiles-playbook) inspired by [geerlingguy/mac-dev-playbook](https://github.com/geerlingguy/mac-dev-playbook) and [dotfiles Ansible role](https://github.com/geerlingguy/ansible-role-dotfiles) which installs and configures machines I use from base installation using package managers like apt, dnf, nix, chocolately, and scoop.
