;;; $DOOMDIR/modules/jt/minimal/config.el -*- lexical-binding: t; -*-
;; Minimal configurations that works with default Emacs


;; * Post Setup  ----------------------------
(after! projectile

  ;; Abbrev Mode to expand abbreviations for shorthand
  ;; http://xahlee.info/emacs/emacs/emacs_abbrev_mode_tutorial.html
  ;; Turn on abbrev mode globally
  ;; (setq-default abbrev-mode t)
  ;; Add abbrev in org mode
  (add-hook 'org-mode-hook #'abbrev-mode)

  ;; Auto save on exit
  ;; by default to Emacs config directory/abbrev_defs
  (setq save-abbrevs 'silently)
  ;; Store abbreviations outside of Doom Emacs folder
  (setq abbrev-file-name "~/.cache/abbrev_defs")

  ;; Temporary load abbreviations from source file
  (setq jt-abbrev-el-location "~/Code/dotfiles/.config/emacs/setup/jt-emacs-abbrev.el")
  (if (file-exists-p jt-abbrev-el-location)
      (load jt-abbrev-el-location)
    )

  ;; Wrap text in all buffers
  (setq-default visual-line-mode t)
  (global-visual-line-mode 1)

  ;; Do not prompt on exit
  (setq confirm-kill-emacs nil)

  ;; --------------------------------------------------------------------------------
  ;; * Emacs Web Wowser - eww web browser  ----------------------------

  ;; Fix background colour contrast
  ;; https://emacs.stackexchange.com/questions/2955/how-to-customize-background-color-for-some-web-pages-opened-with-eww
  (setq shr-color-visible-luminance-min 70)

  ;; --------------------------------------------------------------------------------
  ;; * Dired - File Management ----------------------------

  ;; Previous setting "-alhgo --group-directories-first"
  (setq dired-listing-switches "-alh --group-directories-first")

  ;; Dired toggle display of users and groups
  (defun dired-toggle-user-group-in-listing ()
    "Toggle display of user and group information in dired"
    (interactive)
    (if (string-match-p "ahgo" dired-listing-switches)
        (setq dired-listing-switches "-alh --group-directories-first")
      (setq dired-listing-switches "-alhgo --group-directories-first"))
    (revert-buffer)
    )

  ;; Add hook, when dired buffer is open, active (dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  ;; Turn off hl-line-mode in dired due to a lag until determined what is cause
  (add-hook 'dired-mode-hook (lambda () (hl-line-mode -1)))

  ;; Optionally post set up Emacs configurations
  ;; for example other machine specific configurations
  (setq postsetup-el-location "~/.config/doom/setup/post-setup.el")
  (if (file-exists-p postsetup-el-location)
      (load postsetup-el-location)
    )
  ;; --------------------------------------------------------------------------------
  ;; * Overlay  ----------------------------
  ;; Optionally load dotfiles overlay Emacs configurations
  (setq env-el-location "~/.config/doom/setup/env.el")
  (if (file-exists-p env-el-location)
      (load env-el-location)
    )  )
