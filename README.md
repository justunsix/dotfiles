# JT's Dotfiles

Configuration and customization files to personalize Linux and Windows.

This repository contains my personal dotfiles like configurations and setups.

## Repository Structure

/ 
├── .config/ configuration files for the below mentioned programs
    ├── Code/User - [Visual Studio Code](https://code.visualstudio.com/) (Editor)
    ├── cool-retro-term - [Cool Retro Term](https://github.com/Swordfish90/cool-retro-term) (Terminal Emulator)
	├── copyq - [copyq](https://hluk.github.io/CopyQ/) (Clipboard Manager)
	├── dunst - [dunst](https://dunst-project.org/) (Notification daemon, for use with i3)
	├── emacs - [Emacs](https://www.gnu.org/software/emacs/) (Editor)
	├── fontconfig/conf.d - Font managed by [Nix package manager](https://nixos.org/)
	├── i3 - [i3 Window Manager (WM)](https://i3wm.org/)
	├── i3status - [i3status](https://i3wm.org/docs/i3status.html) (Status bar for use with i3)
	├── mpv - [mpv](https://mpv.io/) (Media Player)
	├── rofi - [rofi](https://github.com/davatorium/rofi) (Application launcher, Window switcher for use with i3)
  	├── todotxt-cli - [todotxt](https://github.com/todotxt/todo.txt-cli) (Task manager)
	├── `kdeglobals` - [KDE](https://kde.org/) (Desktop Environment appearance settings))
	├── `starship.toml` - [Starship](https://starship.rs/) (Cross-shell prompt)
├── .fonts - Open source fonts
├── .local/share/applications - [Desktop entries](https://wiki.archlinux.org/title/desktop_entries)
├── Templates - New file templates for [GNOME File manager](https://wiki.gnome.org/action/show/Apps/Files?action=show&redirect=Apps%2FNautilus) also known as Nautilus
├── usr/bin - Linux scripts
├── usr/bin-windws - Windows scripts
├── other - Other configuration files not normally stored in user home directory
├── `.bash...` - [Bash](https://www.gnu.org/software/bash/) shell configuration files
├── `.gitconfig` - [Git](https://git-scm.com/) distributed version control configuration file
├── `.inputrc` - [GNU Readline](https://tiswww.cwru.edu/php/chet/readline/rltop.html) for command line editing

## How Dotfiles are Managed

There are many ways to manage your dotfiles. I use Ansible inspired by [geerlingguy/mac-dev-playbook](https://github.com/geerlingguy/mac-dev-playbook) which also installs and configures machines I use from initial installation using package managers like apt, dnf, deb-get, chocolately, and nix.

## Programs and Software Configured

### Operating System and Environment

- Operating systems/distributions:
  - [GNU/Linux Ubuntu using GNOME](https://ubuntu.com/desktop)
  - [GNU/Linux Fedora using i3 spin](https://spins.fedoraproject.org/en/i3/)
  - [Microsoft Windows](https://www.microsoft.com/en-ca/windows)
- Desktop environment: [GNOME](https://www.gnome.org/)
- Window manager (WM): [i3 tiling WM](https://i3wm.org/)
  - Status bar: [i3status](https://i3wm.org/docs/i3status.html)

### Shells

- Shell:
  - [Bash](https://www.gnu.org/software/bash/)
    - [GNU Readline](https://tiswww.cwru.edu/php/chet/readline/rltop.html) library
  - [Fish](https://fishshell.com/)
  - Prompt: [Starship](https://starship.rs/)

### Programs

- Version control: [Git](https://git-scm.com/)
- Editor:
  - [Emacs](https://www.gnu.org/software/emacs/)
  - [Visual Studio Code](https://code.visualstudio.com/)
- Video player: [mpv](https://mpv.io/)

- Window switcher/application launcher: [rofi](https://github.com/davatorium/rofi)
- Clipboard manager: [copyq](https://hluk.github.io/CopyQ/)
- Terminal emulator: [Cool Retro Term](https://github.com/Swordfish90/cool-retro-term)
- Notification daemon: [dunst](https://dunst-project.org/)
- Task management: [todotxt](https://github.com/todotxt/todo.txt-cli)
