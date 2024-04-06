;;; $DOOMDIR/modules/jt/minimal/config.el -*- lexical-binding: t; -*-
;; Minimal configurations that works with default Emacs

;; Abbrev Mode to expand abbreviations for shorthand
;; http://xahlee.info/emacs/emacs/emacs_abbrev_mode_tutorial.html
;; Turn on abbrev mode globally
(setq-default abbrev-mode t)
;; Auto save on exit
;; by default to Emacs config directory/abbrev_defs
(setq save-abbrevs 'silently)
;; Store abbreviations outside of Doom Emacs folder
(setq abbrev-file-name "~/.cache/abbrev_defs")

;; Wrap text in all buffers
(setq-default visual-line-mode t)
(global-visual-line-mode 1)

;; Do not prompt on exit
(setq confirm-kill-emacs nil)

;; --------------------------------------------------------------------------------
;; * Overlay  ----------------------------
;; Optionally load dotfiles overlay Emacs configurations
(setq env-el-location "~/.config/doom/setup/env.el")
(if (file-exists-p env-el-location)
		(load env-el-location)
	)

;; * Post Setup  ----------------------------
;; Optionally post set up Emacs configurations
;; for example other machine specific configurations
(setq postsetup-el-location "~/.config/doom/setup/post-setup.el")
(if (file-exists-p postsetup-el-location)
		(load postsetup-el-location)
	)
