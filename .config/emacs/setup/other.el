;;; other.el --- Emacs 3rd Party Packages and Configurations
;;  Author: Justin Tung
;;; Commentary:
;; Emacs configuration with packages in addition to Emacs defaults
;;; Code:

;; --------------------------------------------------------------------------------
;; * System Variables ----------------------------

(defvar jt/linux-p (string= "gnu/linux" system-type))
(defvar jt/windows-p (string= "windows-nt" system-type))

;; --------------------------------------------------------------------------------
;; * Package Manager Configuration ----------------------------

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; All use-package will automatically install the package
(setq use-package-always-ensure t)
;; Verbose useful for debugging use-package loading
(setq use-package-verbose t)

;; Always defer packages unless explicitly demanded on startup
;; In case of errors, force loading on packages with `demand t`
(setq user-package-always-defer t)

;; --------------------------------------------------------------------------------
;; * Package Updates ----------------------------

;; Configure packages to automatically update
(use-package auto-package-update
  ;; Defer 20 seconds after startup
  :defer 20
  :custom
  ;; Interval in days
  (auto-package-update-interval 30)
  ;; Ask before auto update
  (auto-package-update-prompt-before-update t)
  ;; Show results, default is nil
  (auto-package-update-hide-results nil)
  :config
  ;; Check if interval passed
  (auto-package-update-maybe)
  ;; 2nd check in case you rarely restart Emacs
  ;; Around 7pm, check for updates
  ;; (auto-package-update-at-time "19:20"))
	;; Trigger explicitly `M-x auto-package-update-now` to update

;; --------------------------------------------------------------------------------
;; * Support Packages: Search, Help, Projects ----------------------------

;; Inline file fuzzy search
;; Defer loading until swiper is called
(use-package swiper
  :commands swiper)

;; * Help Support ----------------------------

;; Show keys available for pressed commands
(use-package which-key
  ;; Defer to after startup
  :defer 0
  ;; init called before package is loaded
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  ;; Delay before keys show up
	(setq which-key-idle-delay 0.3
				which-key-idle-secondary-delay 0.3)
)

;; Gives better help, code, references, values. Rebind commands to better versions
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  ;; set custom variables instead of setting them manually in emacs
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ;; remap keys bound to a function to the counsel / helpful function instead
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; * Projects -----------------------------

;; Projectile - source control repository project management
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  )

;; NOTE: Set this to the folder where you keep your Git repositories and projects
(setq projectile-project-search-path jt/project-search-path)

(setq projectile-switch-project-action #'projectile-dired)

(projectile-mode +1)
;; Recommended keymap prefix on macOS
;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; --------------------------------------------------------------------------------
;; * Development Configuration ----------------------------

;; --------------------------------------------------------------------------------
;; * Magit git interface ----------------------------

(use-package magit
  ;; Defer magit load to when it is called
  :commands magit-status
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status))
  :config
  ;; For use with magit-list-repositories
  ;; - List of directories that are or contain Git repositories.
  ;; - Format: directory . depth of directory where 0 is just directory itself
  (setq magit-repository-directories
        '(("~/Code/" . 1)
          ("~/Code/External" . 1)))
  ;; - Customize format of magit-list-repositories
  (setq magit-repolist-columns
        '(("Name"    25 magit-repolist-column-ident ())
          ("Version" 25 magit-repolist-column-version ())
					("Flag" 3 magit-repolist-column-flag ())
          ;; ("D"        1 magit-repolist-column-dirty ())
          ("B<U"      3 magit-repolist-column-unpulled-from-upstream
       ((:right-align t)
        (:help-echo "Upstream changes not in branch")))
      ("B>U"      3 magit-repolist-column-unpushed-to-upstream
       ((:right-align t)
        (:help-echo "Local changes not in upstream")))
      ("Path"    99 magit-repolist-column-path ())))
  )

;; --------------------------------------------------------------------------------
;; * Company Auto-Completion  ----------------------------

;; Company-mode completion interface
;; with enhancements from https://github.com/ianyepan/yay-evil-emacs/blob/master/config.org#company-for-auto-completion
(use-package company
	:diminish company-mode
  ;; :after lsp-mode
	:hook (prog-mode . company-mode)
  ;; Use tab to complete selections and initiate completions if needed, by default
  ;; tab is mapped to company-next (next suggestion)
																				;: bind (:map company-active-map
  ;;       ("<tab>" . company-complete-selection))
  ;;      (:map lsp-mode-map
  ;;       ("<tab>" . company-indent-or-complete-common))
	:config
  (setq
	 ;; Triggers for when completions show up
   ;; How many characters must be typed
	 company-minimum-prefix-length 1
	 ;; How long for completion to come
   company-idle-delay 0.1
   company-selection-wrap-around t
   company-tooltip-align-annotations t
   company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                       company-echo-metadata-frontend))
	(define-key company-active-map (kbd "C-n") 'company-select-next)
	(define-key company-active-map (kbd "C-p") 'company-select-previous)
	)

;; Enhance look of completion options
(use-package company-box
  :hook (company-mode . company-box-mode))

;; --------------------------------------------------------------------------------
;; * Key Bindings ----------------------------

;; magit-status with F6
(global-set-key [f6] 'magit-status)

;; Find files in a project - overwrite minimal settings
(global-set-key (kbd "C-p") 'projectile-find-file)

;; Buffer Management
;; Bind jt/kill-all-buffers to F2 k
(global-set-key (kbd "<f2> k") 'jt/kill-all-buffers)

;; Other
(global-set-key (kbd "<f2> m b") 'jt/bongo-open-my-playlist)

;; --------------------------------------------------------------------------------
;; * Other Packages ----------------------------

;; * Dashboard ----------------------------

;; Extensible emacs startup screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-center-content t
      ; Set project package in use
      dashboard-projects-backend 'projectile
      ; Add navigator
      dashboard-set-navigator t
       ; Display packages and load time
      dashboard-set-init-info t)
(setq dashboard-items '((recents  . 10)
                        (projects . 8)
                        (bookmarks . 20)
                        ; (agenda . 5)
                        ))

;; * Bongo ----------------------------

(when jt/linux-p
  (use-package bongo
    :commands bongo
    )

	(defun jt/bongo-open-my-playlist()
		"Open my playlist in bongo stored in playlist environment variable"
		(interactive)
		(bongo)
		(bongo-insert-playlist-contents jt/bongo-playlist-location)
		(bongo-playlist-mode)
		)
  )

;; Always play on random
;; (bongo-random-playback-mode)

;; --------------------------------------------------------------------------------
;; * Changes adapted from Yay-Evil emacs distro by Ian Y.E. Pan -------------------
;; Licensed under GPL3 from https://github.com/ianyepan/yay-evil-emacs/tree/master
;; 3rd Party Emacs Packages Section

;; Syntax highlighting
;; Lightweight syntax highlighting improvement for numbers and escape sequences (e.g. \n, \t).
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

;; On-the-fly syntax checking extension
(use-package flycheck
	:config (global-flycheck-mode +1)
	)

;; --------------------------------------------------------------------------------
;; * System Runtime ----------------------------

;; --------------------------------------------------------------------------------
;; * Custom Functions ----------------------------

(defun jt/kill-all-buffers-except-starred ()
  "Kill all buffers except ones starting with * and switch to *dashboard*."
  (interactive)
	;; Switch to buffer called *dashboard*
  (switch-to-buffer "*dashboard*")
  (kill-matching-buffers "^[^*]" nil t)
	)

(defun jt/switch-to-buffer-dashboard ()
	"Switch to buffer called *dashboard*."
	(interactive)
	(switch-to-buffer "*dashboard*")
	)

(provide 'other)

;;; other.el ends here
