;; Abbrev Mode to expand abbreviations for shorthand
;; http://xahlee.info/emacs/emacs/emacs_abbrev_mode_tutorial.html
;; Turn on abbrev mode globally
(setq-default abbrev-mode t)
;; Auto save on exit
;; by default to Emacs config directory/abbrev_defs
(setq save-abbrevs 'silently)

;; * Post Setup  ----------------------------
;; Optionally post set up Emacs configurations
;; for example other machine specific configurations
(setq postsetup-el-location "~/.config/emacs/setup/post-setup.el")
(if (file-exists-p postsetup-el-location)
		(load postsetup-el-location)
	)