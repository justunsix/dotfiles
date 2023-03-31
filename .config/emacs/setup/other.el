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
  (auto-package-update-interval 7)
  ;; Ask before auto update
  (auto-package-update-prompt-before-update t)
  ;; Show results, default is nil
  (auto-package-update-hide-results nil)
  :config
  ;; Check if interval passed
  (auto-package-update-maybe)
  ;; Around 7pm, check for updates
  ;; 2nd check in case you rarely restart Emacs
  (auto-package-update-at-time "19:20"))
;; Trigger explicitly `M-x auto-package-update-now` to update

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

;; --------------------------------------------------------------------------------
;; * Font Configuration ----------------------------

;; You will most likely need to adjust this font size for your system
(defvar efs/default-font-size 120)

;; Function to set default fonts
(defun jt/set-default-fonts ()
  "Set the font size based on the defaults font size."
  (interactive)
  (set-face-attribute 'default nil :font "Source Code Pro" :weight 'normal :height efs/default-font-size)
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :weight 'normal :height 115)
  ;; Set the variable pitch face with weight normal
  (set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :weight 'normal :height efs/default-font-size)
  )

(jt/set-default-fonts)

;; Set tab bar inactive face with background colour black
(set-face-attribute 'tab-bar-tab-inactive nil :background "black")
;; Set active tab background colour
(set-face-attribute 'tab-bar-tab nil :background "gray25")

;; --------------------------------------------------------------------------------
;; * Support Packages: Search, Completion ----------------------------

;; Inline file fuzzy search
;; Defer loading until swiper is called
(use-package swiper
  :commands swiper)

;; --------------------------------------------------------------------------------
;; * Counsel ----------------------------
;; Navigation Management
;; Rebind to counsel functions
(use-package counsel
  :bind (("M-x" . 'counsel-M-x)
         ("C-x b" . 'counsel-switch-buffer)
         ("C-x C-f" . 'counsel-find-file))
  :config
  (counsel-mode 1)
  )

;; --------------------------------------------------------------------------------
;; * Ivy - Completion framework ----------------------------
(use-package ivy
  ;; minor mode name will not display in mode line but will still be active
  :diminish
  :bind (("C-f" . swiper) ;; Common find binding
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  ;; Set up keybindings
  (ivy-mode 1))

;; Remove ^ which Ivy uses as a default input in counsel-M-x
;; so search will be by substring rather than requiring the command
;; to begin with the first search string
;; https://emacs.stackexchange.com/questions/38841/counsel-m-x-always-shows
(setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")

;; Completion descriptions for commands
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; For Ivy completions, ignore order for regex to allow matches
;; to be in any order. See optiosn and defaults at https://oremacs.com/swiper/
(setq ivy-re-builders-alist
    '((t . ivy--regex-ignore-order)))

;; Show most used commands using M-x, used by counsel M-x
(use-package amx
  :after counsel)

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

;; * Help Support ----------------------------

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
;; * Dired - File Management ----------------------------

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  ;; Display directories first, remove user and group information
  )

;; Bind wdired-change-to-wdired-mode to W in dired-mode
(define-key dired-mode-map (kbd "W") 'wdired-change-to-wdired-mode)

;; File icons in dired mode if graphical environment
;; Use treemacs icons since they are nicer instead of all-the-icons-dired, faster
;; https://emacs.stackexchange.com/questions/71269/all-the-icons-are-all-white-in-dired
(use-package treemacs-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . treemacs-icons-dired-mode))

;; Open files in dired with external programs
(use-package dired-open
  :after dired
  :config
  ;; does not work yet in Linux?
  ;; (add-to-list 'dired-open-functions #'dired-open-xdg)
  (setq dired-open-extensions '(("mp4" . "mpv")
                                ("avi" . "mpv")
                                ("wmv" . "mpv")
                                ("webm" . "mpv")
                                ("png" . "gimp")
                                ("pdf" . "firefox")
                                ("html" . "firefox")
                                ("xlsx" . "libreoffice")
                                ("docx" . "libreoffice")
                                ("pptx" . "libreoffice")
                                ("odt" . "libreoffice")
                                ("ods" . "libreoffice")
                                )
        )
  )

;; dired-recent - History of paths visited with Emacs dired.
(use-package dired-recent
  :after dired
  :config
  (dired-recent-mode 1)
  )
;; dired-recent-mode hijacks the C-x C-d key that in theory it shouldn’t, default bind is list-directory
;; you can unbind it with the following code. Note that dired-recent must be already loaded.
;; (define-key dired-recent-mode-map (kbd "C-x C-d") nil)
;; (define-key dired-recent-mode-map (kbd "SOME OTHER KEY") #'dired-recent-open)

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
        x-underline-at-descent-line t)
  ;; Make the headline face match the centaur-tabs-default face. This makes the tabbar have an uniform appearance
  (centaur-tabs-headline-match)
  ;; Cycle options: tabs, tab groups, just groups
  ;; (setq centaur-tabs-cycle-scope 'tabs)
  ;; Fix length of tabs, tab text can be abbreviated
  ;; (setq centaur-tabs-label-fixed-length 30)
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
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          ;; org-src-mode
                          ;; org-agenda-mode
                          ;; org-beamer-mode
                          ;; org-indent-mode
                          ;; org-bullets-mode
                          ;; org-cdlatex-mode
                          ;; org-agenda-log-mode
                          ;; diary-mode
                          ))
       "OrgMode")
      ((memq major-mode '(term-mode
                          eshell-mode))
       "Terminal")
      (t
       (centaur-tabs-get-group-name (current-buffer)))
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

;; --------------------------------------------------------------------------------
;; * Org Mode Setup --------------------------

;; Set up org mode
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (jt/copilot)
  )

;; hook to improve org mode display
(use-package org
  :hook (org-mode . efs/org-mode-setup)
  ;; Load org package when these other commands are called
  :commands (org-capture org-agenda)
  :config
  ;; Debug message to check when org mode is loading
  ;; (message "Loading Org Mode...")
  ;; Instead of ellipsis in headings, use this value
  (setq org-ellipsis " ▾")
  (efs/org-font-setup)
  :bind (("C-c a" . org-agenda)
         ;; Bind C-c l to my-org-insert-link similar to org-insert-link
         ("C-c l" . my-org-insert-link)
				 ;; Bind org-insert-heading
				 ("C-c h" . org-insert-heading)
         ;; Override org-mode's org-open-at-point with jt/org-open-at-point
				 ;; This allows for opening links in the same window
				 (:map org-mode-map
							 ("C-c C-o" . jt/org-open-at-point))
				 )
	)

;; Add where are your org files
(setq org-directory jt/org-directory)
;; (setq org-agenda-files jt/org-directory)

;; Enable logs of recent activity
(setq org-agenda-start-with-log-mode t)
;; When task is done, log time
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

;; Prepare for image resizing
(setq org-image-actual-width t)

;; Set org priorities ranging from A to J
(setq  org-enable-priority-commands t
       org-highest-priority ?A
       org-default-priority ?J
       org-lowest-priority ?J
       )

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  ;;(font-lock-add-keywords 'org-mode
  ;;                        '(("^ *\\([-]\\) "
  ;;                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levelsq
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Source Code Pro" :weight 'regular :height (cdr face)))

  ;; Set org-level-3 font face to be lighter green
  (set-face-attribute 'org-level-3 nil :foreground "#00ff00")
  ;; Set org-level-4 font face to be lighter blue
	(set-face-attribute 'org-level-4 nil :foreground "#56a6a9")

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

																				; Replace starts with different types of bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Center text on screen in org
;;  (defun efs/org-mode-visual-fill ()
;;  (setq visual-fill-column-width 230
;;       visual-fill-column-center-text t)
;;  (visual-fill-column-mode 1))

;;(use-package visual-fill-column
;;  :hook (org-mode . efs/org-mode-visual-fill))

;; Automatically tangle our literate org dotfiles when we save them
(defun efs/org-babel-tangle-config ()
  ;; Check if current buffer-file-name string contains word "Dotfile"
  ;; If it contains "Dotfile", run tangle with no warning prompt
  (when (string-match-p "Dotfile" (buffer-file-name))
		(let ((org-confirm-babel-evaluate nil))
			(org-babel-tangle))
    )
  )

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

;; Defer after org is loaded
(with-eval-after-load 'org

	;; Configure Babel recognized languages:
	;; elips, python, shell (includes bash)
	;; See list https://orgmode.org/worg/org-contrib/babel/languages/index.html
	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((emacs-lisp . t)
		 (python . t)
		 (shell . t)
		 (sql . t)
		 ;; plantuml
		 ;; per https://plantuml.com/emacs
		 (plantuml . t)
		 ;; (haskell . t)
		 ;;	(powershell . t)
		 )
	 )
	(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
	)

;; Defer after org is loaded
(with-eval-after-load 'org

  ;; Structure Templates
  ;; For org-babel source, templates for langauge e.g. <el + TAB starts template... >
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("txt" . "src txt"))
  (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))
  (add-to-list 'org-structure-template-alist '("puml" . "src plantuml"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
	;; Hashicorp Configuration Language (HCL)
  (add-to-list 'org-structure-template-alist '("hcl" . "src hcl"))
	(add-to-list 'org-structure-template-alist '("ps1" . "src powershell"))
  ;;  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  ;;  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  ;;  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  ;;  (add-to-list 'org-structure-template-alist '("go" . "src go"))

	(add-to-list
	 'org-src-lang-modes '("plantuml" . plantuml))

  (setq org-plantuml-jar-path "~/.config/emacs/plantuml/plantuml.jar")

  )

;; --------------------------------------------------------------------------------
;; * Org Links --------------------------

;; --- Org mode hacks from:
;; https://orgmode.org/worg/org-hacks.html
;; Insert link with HTML title as default description
;;  When using `org-insert-link' (`C-c C-l') it might be useful to extract contents from HTML <title> tag and use it as a default link description.
(require 'mm-url) ; to include mm-url-decode-entities-string

(defun my-org-insert-link ()
  "Insert org link where default description is set to html title."
  (interactive)
  (let* ((url (read-string "URL: "))
         (title (get-html-title-from-url url)))
    (org-insert-link nil url title)))

(defun get-html-title-from-url (url)
  "Return content in <title> tag."
  (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      (beginning-of-buffer)
      (setq x1 (search-forward "<title>"))
      (search-forward "</title>")
      (setq x2 (search-backward "<"))
      (mm-url-decode-entities-string (buffer-substring-no-properties x1 x2)))))

;; Disable org-roam on Windows
(unless jt/windows-p

  ;; --------------------------------------------------------------------------------
  ;; * Org Roam --------------------------
  ;; Most configuration from
  ;; - https://systemcrafters.net/build-a-second-brain-in-emacs/capturing-notes-efficiently/
  ;; - https://www.youtube.com/watch?v=YxgA5z2R08I
  ;; - https://github.com/nobiot/md-roam/blob/main/README.md
  ;; - Recommended: https://github.com/org-roam/org-roam#configuration

  ;; Set org-roam-directory before org-roam loads
  ;; https://github.com/doomemacs/doomemacs/issues/4130
  (setq org-roam-directory (file-truename jt/org-directory))

  (use-package org-roam
    :ensure t
    :custom
    ;; Allow linking outside of org files
    (org-roam-completion-everywhere t)
    (org-roam-capture-templates
     '(("d" "default" plain
        "%?"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
        :unnarrowed t)))
    ;; bindings, use C-c n since it is not bound
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ;; Dailies
           ("C-c n j" . org-roam-dailies-capture-today))
    :config
    ;; If you're using a vertical completion framework, you might want a more informative completion interface
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    ;; Sync org-roam database
    (org-roam-db-autosync-mode)
    ;; If using org-roam-protocol
    ;;  (require 'org-roam-protocol)
    )

  ;; Synchronize database on startup
  (org-roam-db-sync)

  ;; Run org-roam-update-id-locations to rebuild ids to files per
  ;; https://github.com/org-roam/org-roam/issues/1702

  ;; visual queue about regular links vs org-roam links
  ;; https://wiki.systemcrafters.net/emacs/org-roam/
  ;; not working in version 2
  ;;(custom-set-faces
  ;; '((org-roam-link org-roam-link-current)
  ;;   :foreground "#e24888" :underline t))
  )

;;  (use-package org-preview-html
;;    :after org
;;    )

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

;; Add rainbow delimiters for paratheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Tree view of code like symbols, diagnostic messages (errors/warnings)
(use-package lsp-treemacs
  :after lsp
	:config
	;; Reduce font size used in the treemacs buffer, useful for long file names
	(set-face-attribute 'treemacs-file-face nil :height 80)
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
;; * Language and Language Servers ----------------------------

; Breadcrumb to show structure of file and where you are located
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; lsp and lsp-deferred will activate lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
	;; set personal lsp-mode feature prefix to overwrite default Super key + L
	;; Or 'C-l', 's-l'
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; yasnippet for lsp
(use-package yasnippet
	:hook (lsp-mode . yas-minor-mode))

;; UI enhancements for LSP
;; Can use sideline configuration for more frame help on screen
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Integrate Ivy with lsp-mode to search in code, like symbols
(use-package lsp-ivy
  ;; Defer to after lsp is loaded
  :after lsp)

;; TypeScript mode is activated when .ts file is opened
(use-package typescript-mode
  :mode "\\.ts\\'"
  ; Activate lsp-deferred, deferred so LSP activates only when .ts file is active buffer
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Defer LSP to start later, activate visual line (word wrap)
;; and remove line numbers
(defun efs/markdown-mode-setup ()
	(lsp-deferred)
	(visual-line-mode 1)
	(setq display-line-numbers nil)
	;; Spell check
	(flyspell-mode 1)

	(jt/copilot)
	)
;; Markdown support, syntax highlighting
(use-package markdown-mode
	:ensure t
	:hook (markdown-mode . efs/markdown-mode-setup)
	:mode ("README\\.md\\'" . gfm-mode)
	:init (setq markdown-command "multimarkdown"
							markdown-fontify-code-blocks-natively t)
	:config
	(require 'lsp-marksman)
	)

(use-package yaml-mode
  :hook (yaml-mode)
  :mode "\\.yaml\\'"
      )

(use-package plantuml-mode
	:mode "\\.plantuml\\'"
	:config
	(setq plantuml-executable-path "/usr/bin/plantuml")
	(setq plantuml-default-exec-mode 'executable)
	(setq plantuml-output-type "png")
	)

(use-package json-mode
  :mode "\\.json\\'"
  :hook (json-mode . lsp-mode))

(use-package json-reformat
  :after json-mode
  :bind (:map json-mode-map
         ("C-c C-f" . json-reformat-region)))

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . lsp-mode))

(use-package hcl-mode
	:mode "\\.hcl\\'")

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2) ; HTML
  (setq web-mode-css-indent-offset 2)    ; CSS
  (setq web-mode-code-indent-offset 2)   ; JS/JSX/TS/TSX
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package powershell
	:mode "\\.ps1\\'")

;; --------------------------------------------------------------------------------
;; * Key Bindings ----------------------------

;; magit-status with F6
(global-set-key [f6] 'magit-status)

;; VS Code Like Acccessible Commands
;; Call commands with F1
(global-set-key [f1] 'counsel-M-x)

;; Find files in a project - overwrite minimal settings
(global-set-key (kbd "C-p") 'projectile-find-file)

;; Browse recently opened files - overwrite minimal settings
(global-set-key (kbd "C-r") 'counsel-recentf)

;; Buffer Management
;; Bind jt/kill-all-buffers to F2 k
(global-set-key (kbd "<f2> k") 'jt/kill-all-buffers)

;; Other
(global-set-key (kbd "<f2> m b") 'jt/bongo-open-my-playlist)
;; Bind org-toggle-link-display to F2 l
(global-set-key (kbd "<f2> l") 'org-toggle-link-display)

;; Bind switch buffer to C-tab - overwrite minimal settings
(global-set-key (kbd "<C-tab>") 'counsel-switch-buffer)

;; Bind mark-whole-buffer aka select all to C-a
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; --------------------------------------------------------------------------------
;; * Terminals ----------------------------

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  ;; to prevent commands from being lost
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for improve  performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  ;(evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  ;(evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  ;(evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        ; Only scroll to bottom on input, output can run and shell stays in same place
        eshell-scroll-to-bottom-on-input t))


(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
	)

;; --------------------------------------------------------------------------------
;; * Spelling - Writing: Languages Spelling ----------------------------
;; from https://github.com/munen/emacs.d/#flyspell

;; Order corrections by likeliness
;; Do not order not by the default of alphabetical ordering.
(setq flyspell-sort-corrections nil)

;; Do not print messages for every word
;; When checking the entire buffer, don’t print messages for every word. This is a major performance gain.
(setq flyspell-issue-message-flag nil)

;; if Windows, set ispell-hunspell-dict-paths-alist to "C:/Hunspell"
(when jt/windows-p
	(setq ispell-hunspell-dict-paths-alist '(("en_US" "C:/Hunspell/en_US.aff" "C:/Hunspell/en_US.dic")
																					 ("en_GB" "C:/Hunspell/en_GB.aff" "C:/Hunspell/en_GB.dic")
																					 ("fr_FR" "C:/Hunspell/fr.aff" "C:/Hunspell/fr.dic")
																					 ("fr_CA" "C:/Hunspell/fr.aff" "C:/Hunspell/fr.dic")
																					 ))
	)

;; hunspell with multiple dictionaries
;; Ensure dictories are installed with OS package managers
(with-eval-after-load "ispell"
	;; Configure `LANG`, otherwise ispell.el cannot find a 'default
	;; dictionary' even though multiple dictionaries will be configured
	;; in next line.
	(setenv "LANG" "en_US.UTF-8")
	(setq ispell-program-name "hunspell")
	;; Configure US, British variants of English, International and Canadian French
	(setq ispell-dictionary "en_GB,en_US,fr_FR,fr_CA")
	;; ispell-set-spellchecker-params has to be called
	;; before ispell-hunspell-add-multi-dic will work
	(ispell-set-spellchecker-params)
	(ispell-hunspell-add-multi-dic "en_GB,en_US,fr_FR,fr_CA")
	;; For saving words to the personal dictionary, don't infer it from
	;; the locale, otherwise it would save to ~/.hunspell_de_DE.
	;; The personal dictionary file has to exist, otherwise hunspell will silently not use it.
	;; Make sure file is present

	(ispell-change-dictionary "en_GB,en_US,fr_FR,fr_CA")
	(setq ispell-personal-dictionary "~/.config/hunspell/.hunspell_personal")
	)
;; Run flyspell-mode in org-mode and markdown-mode
;; where spelling assistance is needed
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

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
;; * Straight Package Management ----------------------------

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

;; If the day is Sunday, run (straight-pull-all)
;; Only run if want to grab updates
;; (when (equal (nth 6 (decode-time)) 0)
;;  (straight-pull-all))

;; --------------------------------------------------------------------------------
;; * Copilot ----------------------------

(defun jt/copilot ()
  ;; Activate copilot only under certain conditions, previously only if proxy not present.
  (copilot-mode 1)
  )

;; Installation and instructions at https://github.com/zerolfx/copilot.el
(setq copilot-node-executable "~/.nvm/versions/node/v18.14.0/bin/node")

(when jt/windows-p
	;; Set to nodejs location installed by Chocolately
	;; Deprecated:
	;; - manually nodejs v17 folder structure using
  ;; - Windows 64 bit binary from https://nodejs.org/download/release/v17.9.1/
  ;; - as if it was installed by nvm
  (setq copilot-node-executable "C:/Program Files/nodejs/node.exe")
  )

;; Use straight.el for use-package expressions
;; (straight-use-package 'use-package)

;; Load copilot using straight
;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t)

;; Load copilot using straight
(straight-use-package
 '(copilot :type git :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")))

;; Active copilot-mode on org-mode, markdown-mode, yaml-mode, Elisp/d mode, sh-mode
(add-hook 'org-mode-hook 'copilot-mode)
(add-hook 'markdown-mode-hook 'copilot-mode)
(add-hook 'yaml-mode-hook 'copilot-mode)
(add-hook 'emacs-lisp-mode-hook 'copilot-mode)
(add-hook 'sh-mode-hook 'copilot-mode)

;; (add-hook 'prog-mode-hook 'copilot-mode)

;; Ensure copilot is loaded
(require 'copilot)

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; Wait in seconds before suggesting completion
(setq copilot-idle-delay 0.4)

;; Set copilot-overlay-face to inherit from font-lock-comment-face
;; Default is `shadow` which is too light
(set-face-attribute 'copilot-overlay-face nil
                    :inherit 'font-lock-comment-face)

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

(defun jt/kill-all-buffers ()
  "Kill all buffers except *dashboard*, *scratch*, *Messages* and if buffer exists *copilot events*"
  (interactive)
  ;; Switch to buffer called *dashboard*
  (switch-to-buffer "*dashboard*")
  (delete-other-windows)
  (mapc 'kill-buffer (delq (current-buffer) (delq (get-buffer "*dashboard*") (delq (get-buffer "*scratch*") (delq (get-buffer "*Messages*") (delq (get-buffer "*copilot events*") (buffer-list)))))))
  ;; Restart copilot
  (copilot-diagnose)

  ;; Send minibuffer message
  (sleep-for 1)
  (message "All buffers killed except *dashboard*, *scratch*, *Messages*, and *copilot events*, restarting copilot")
  )

(defun jt/switch-to-buffer-dashboard ()
	"Switch to buffer called *dashboard*."
	(interactive)
	(switch-to-buffer "*dashboard*")
	)

;; Customize org-link-frame-setup variable to use find file
;; to prevent org from opening in a new window in frame
;; from: https://stackoverflow.com/questions/1854214/how-do-i-keep-emacs-org-mode-from-splitting-windows/1854647#1854647
(defun jt/org-open-at-point ()
	"Open the link at point in same window, do not split frame."
	(interactive)
	(let ((org-link-frame-setup
				 (list (cons 'file 'find-file))))
		(org-open-at-point))
	)

(defun jt/copy-file-name-to-clipboard-as-org-link ()
	"Copy current file name as an org link to the kill ring (clipboard)."
	(interactive)
	(kill-new (format "[[file:%s]]" (file-name-nondirectory (buffer-file-name))))
	)

(provide 'other)

;;; other.el ends here
