;;; jt-emacs-ui.el --- Personal Emacs UI configurations
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related to Emacs' user interface
;;; Code:

;; --------------------------------------------------------------------------------
;; * User Interface Setup  ----------------------------

;; Settings only for graphical environments
;; not required for terminal mode
(when (display-graphic-p)
	;; Different backgrounds for most visited buffers and "side" buffers like messages
	(use-package solaire-mode)
	(solaire-global-mode +1)
	)

;; --------------------------------------------------------------------------------
;; * Themes  ----------------------------

(use-package doom-themes
	;; Other preferred themes include doom-one, doom-dark+, doom-tokyo-night, doom-vibrant, doom-palenight
	;; Built in theme: tango-dark or wombat
	;; (load-theme 'wombat t)

	:init
	(load-theme 'doom-tokyo-night t))

;; --------------------------------------------------------------------------------
;; * Modeline Configuration ----------------------------

;;Note: The first time you load your configuration on a new machine, you’ll need to run `M-x all-the-icons-install-fonts` so that mode line icons display correctly.

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Icons used by doom-modeline
;; After install, run M-x nerd-icons-install-fonts to install the necessary fonts
;; per https://github.com/seagle0128/doom-modeline
(use-package nerd-icons)

;; --------------------------------------------------------------------------------
;; * Font Configuration ----------------------------

;; You will most likely need to adjust this font size for your system
(defvar efs/default-font-size 110)

;; Function to set default fonts
(defun jt/set-default-fonts ()
  "Set the font size based on the defaults font size."
  (interactive)
  (set-face-attribute 'default nil
											:font "Source Code Pro"
											:weight 'normal
											:height efs/default-font-size)
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
											:font "Source Code Pro"
											:weight 'normal
											:height 115)
  ;; Set the variable pitch face with weight normal
	(when jt/linux-p
		(set-face-attribute 'variable-pitch nil
												:font "JetBrains Mono"
												:weight 'normal
												:height efs/default-font-size)
		)

  (when jt/windows-p
		;; Temporary Fix
    ;; On Windows to Source Code Pro due to problem with JetBrains Mono
		;; If not family, may get error like "Font not available, #<font-spec nil nil JetBrains Mono"
		(set-face-attribute 'variable-pitch nil
												:family "Source Code Pro"
												:weight 'normal
												:height efs/default-font-size)
		)

  )

(jt/set-default-fonts)

;; Set tab bar inactive face with background colour black
(set-face-attribute 'tab-bar-tab-inactive nil :background "black")
;; Set active tab background colour
(set-face-attribute 'tab-bar-tab nil :background "gray25")

;; --------------------------------------------------------------------------------
;; * Tabs ----------------------------
;; from https://github.com/ema2159/centaur-tabs
(use-package centaur-tabs
  :config
  (setq centaur-tabs-style "wave"
        ;; tab bar height
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        ;; Indicate buffer is modified
        centaur-tabs-set-modified-marker t
        ;; Grey out icons for unselected tabs
        ;; (setq centaur-tabs-gray-out-icons 'buffer)
        centaur-tabs-show-navigation-buttons t
        ;; Set location of highlight bar in active tab
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
  		  ;; Fix length of tabs, tab text is abbreviated
				centaur-tabs-label-fixed-length 20
				)
  ;; Make the headline face match the centaur-tabs-default face. This makes the tabbar have an uniform appearance
  (centaur-tabs-headline-match)
  ;; Cycle options: tabs, tab groups, just groups
  ;; (setq centaur-tabs-cycle-scope 'tabs)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      (
       (or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              ))
           )
       "Emacs"
       )
			;;      ((derived-mode-p 'prog-mode)
			;;       "Editing")
			;;      ((derived-mode-p 'dired-mode)
			;;       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
													org-agenda-clockreport-mode
													prog-mode
													text-mode
													dired-mode
													elisp-mode
                          ;; org-src-mode
                          ;; org-agenda-mode
                          ;; org-beamer-mode
                          ;; org-indent-mode
                          ;; org-bullets-mode
                          ;; org-cdlatex-mode
                          ;; org-agenda-log-mode
                          ;; diary-mode
                          ))
       "OrgProgMode")
			;;      ((memq major-mode '(term-mode
			;;                          eshell-mode))
			;;       "Terminal")
      (t
       (centaur-tabs-get-group-name (current-buffer))
			 )
      )
     ) ; end of list
    ) ; end of defun
  :hook
  ;; Disable tabs on these modes when full screen
  (dashboard-mode . centaur-tabs-local-mode)
  ;; (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ;; Backward bound to C-Page Up
  ("C-<prior>" . centaur-tabs-backward)
  ;; Forward  bound to C-Page Down
  ("C-<next>" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ;; Group tabs by project
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  ;; (:map evil-normal-state-map
  ;;      ("g t" . centaur-tabs-forward)
  ;;      ("g T" . centaur-tabs-backward)
  ;;      )
  ) ; end of use package

(centaur-tabs-group-buffer-groups)
(centaur-tabs-change-fonts "Source Code Pro" 80)

;; Add rainbow delimiters for paratheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Tree view of code like symbols, diagnostic messages (errors/warnings)
(use-package lsp-treemacs
  :after lsp
	:config
	;; Reduce font size used in the treemacs buffer, useful for long file names
	(set-face-attribute 'treemacs-file-face nil :height 70)
	)

;; Search
(use-package rg)

(provide 'jt-emacs-ui)

;;; jt-emacs-ui.el ends here
