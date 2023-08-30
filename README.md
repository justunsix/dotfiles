# JT's Dotfiles

Configuration and customization files to personalize Linux and Windows.

This repository contains my personal dotfiles like configurations for software described below.

## Repository Structure

/<br/>
├── `.config/` configuration files for the below mentioned programs<br/>
------├── `Code/User/` - [Visual Studio Code](https://code.visualstudio.com/) Editor<br/>
------├── `cool-retro-term/` - [Cool Retro Term](https://github.com/Swordfish90/cool-retro-term) Terminal Emulator<br/>
------├── `copyq/` - [copyq](https://hluk.github.io/CopyQ/) Clipboard Manager<br/>
------├── `dunst/` - [dunst](https://dunst-project.org/) Notification daemon, for use with i3<br/>
------├── `emacs/` - [Emacs](https://www.gnu.org/software/emacs/) Editor<br/>
------├── `fish/` - [Fish](https://fishshell.com/) shell<br/>
------├── `fontconfig/conf.d/` - Fonts managed by [Nix package manager](https://nixos.org/)<br/>
------├── `i3/` - [i3 Window Manager (WM)](https://i3wm.org/)<br/>
------├── `i3status/` - [i3status](https://i3wm.org/docs/i3status.html) Status bar for use with i3<br/>
------├── `input-remapper/` - [input-remapper](https://github.com/sezanzeb/input-remapper) to change input behaviour like key remapping<br/>
------├── `mprocs/` - [mprocs](https://github.com/pvolok/mprocs) to manage commonly executed processes and manage them<br/>
------├── `mpv/` - [mpv](https://mpv.io/) media player<br/>
------├── `nvim/` - [neovim](https://neovim.io/) Editor<br/>
------├── `rofi/` - [rofi](https://github.com/davatorium/rofi) Application launcher, Window switcher for use with i3 and in GNOME<br/>
------├── `tmux` - [tmux](https://github.com/tmux/tmux/wiki) Terminal Multiplexer<br/>
------├── `todotxt-cli/` - [todotxt](https://github.com/todotxt/todo.txt-cli) Task manager on command line<br/>
------├── `topgrade/` - [topgrade](https://github.com/topgrade-rs/topgrade) Updater for things like operating systems, software, packages, and others<br/>
------├── `vlc/` - [vlc](https://www.videolan.org/vlc/) media player<br/>
------├── `dolphinrc` - [Dolphin File Manager](https://apps.kde.org/dolphin/) settings <br/>
------├── `kdeglobals` - [KDE](https://kde.org/) Desktop Environment appearance settings<br/>
------├── `starship.toml` - [Starship](https://starship.rs/) Cross-shell prompt<br/>
├── `.fonts/` - Open source fonts<br/>
├── `.local/share/applications/` - [Desktop entries](https://wiki.archlinux.org/title/desktop_entries)<br/>
├── `Templates/` - New file templates for [GNOME File manager](https://wiki.gnome.org/action/show/Apps/Files?action=show&redirect=Apps%2FNautilus) also known as Nautilus<br/>
├── `usr/bin/` - Linux scripts<br/>
├── `usr/bin-windows/` - Windows scripts<br/>
├── `other/` - Other configuration files not normally stored in user home directory<br/>
------├── `PowerShell/` - [PowerShell](https://learn.microsoft.com/en-us/powershell/scripting/overview?view=powershell) shell<br/>
------├── `WindowsPowerShell/` - [Windows PowerShell](https://learn.microsoft.com/en-us/powershell/scripting/windows-powershell/starting-windows-powershell) 5.1 Windows shell<br/>
├── `.bash...` - [Bash](https://www.gnu.org/software/bash/) shell<br/>
├── `.gitconfig` - [Git](https://git-scm.com/) distributed version control configuration file<br/>
├── `.inputrc` - [GNU Readline](https://tiswww.cwru.edu/php/chet/readline/rltop.html) for command line editing<br/>
├── `.Xresources` , `.xinitrc` - [X Window System](https://www.x.org/wiki/) configuration files<br/>

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
  - [PowerShell](https://learn.microsoft.com/en-us/powershell/scripting/overview?view=powershell)
  - Prompt: [Starship](https://starship.rs/)

### Programs

- Version control: [Git](https://git-scm.com/)
- Editor:
  - [Emacs](https://www.gnu.org/software/emacs/)
  - [Visual Studio Code](https://code.visualstudio.com/)
  - [neovim](https://neovim.io/)
- Video player: [mpv](https://mpv.io/) and [vlc](https://www.videolan.org/vlc/)

- Window switcher/application launcher: [rofi](https://github.com/davatorium/rofi)
- Clipboard manager: [copyq](https://hluk.github.io/CopyQ/)
- Terminal emulator: [Cool Retro Term](https://github.com/Swordfish90/cool-retro-term)
- Notification daemon: [dunst](https://dunst-project.org/)
- Personal Task management: [todotxt](https://github.com/todotxt/todo.txt-cli)
- Parallel commands manager: [mprocs](https://github.com/pvolok/mprocs)
- Updates: [topgrade](https://github.com/topgrade-rs/topgrade)

## How Dotfiles are Managed

There are many ways to manage your dotfiles. I use an [Ansible playbook](https://github.com/justunsix/dotfiles-playbook) inspired by [geerlingguy/mac-dev-playbook](https://github.com/geerlingguy/mac-dev-playbook) and [dotfiles Ansible role](https://github.com/geerlingguy/ansible-role-dotfiles) which installs and configures machines I use from base installation using package managers like apt, dnf, deb-get, chocolately, and nix.
