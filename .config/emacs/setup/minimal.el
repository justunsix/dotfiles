;;; minimal.el --- Emacs Native Packages Configuration
;;  Author: Justin Tung
;;; Commentary:
;; Minimal configuration that works with default Emacs 28.2+ with no extra packages
;;; Code:

;; --------------------------------------------------------------------------------
;; * Emacs General Settings -------------------------------------------------------

;; Change yes or no to y or n to simplify typing
;; From Sacha Chua https://sachachua.com/dotemacs/index.html#org1f60133
;; (fset 'yes-or-no-p 'y-or-n-p)
;; As of Emacs 28, alternatively set as
(setq use-short-answers t)

;; Auto Save
(auto-save-mode t)
(setq auto-save-default t)

;; When tab is hit, set to 2 spaces
(setq-default tab-width 2)

;; Automatically update files as they change on files,
;; like being edited in another program
;; easier for syncing
(global-auto-revert-mode 1)
(setq-default auto-revert-mode t)
;; Automatically update non-file buffers like dired
(setq global-auto-revert-non-file-buffers t)

;; By default, Emacs saves backup files in the current directory
;; These are the files ending in ~ that are cluttering up your directory lists
;; https://sachachua.com/dotemacs/index.html#org53b3e0c
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))

;; Recent Files
(recentf-mode 1)

(setq recentf-max-saved-items 1000
      recentf-max-menu-items 30)

;; Remember last place you visited in a file
(save-place-mode 1)

;; Disable graphical dialog boxes
(setq use-dialog-box nil
      ;; Shift and arrow keys always select (except for some org functions)
      shift-select-mode t
      ;; Do not blink cursor
      blink-cursor-mode nil
      )

;; Disable graphical dialog boxes
(setq use-dialog-box nil)

;; Set Emacs regexp helper re-builder to string syntax
;; from: https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(setq reb-re-syntax 'string)

;; Abbrev Mode to expand abbreviations for shorthand
;; http://xahlee.info/emacs/emacs/emacs_abbrev_mode_tutorial.html
;; Turn on abbrev mode globally
(setq-default abbrev-mode t)
;; Auto save on exit
;; by default to Emacs config directory/abbrev_defs
(setq save-abbrevs 'silently)

;; Set preferred coding system to utf-8
;;
;; Gets around a possible issue in encoding files and *temp* buffer
;; with message "These default coding systems were tried to encode text in the buffer ` *temp*':
;; ... However, each of them encountered characters it couldn't encode
;; Issue is described at:
;; - https://emacs.stackexchange.com/questions/7162/emacs-doesnt-let-me-exit-without-selecting-a-coding-system
;; - and at with this solution https://www.reddit.com/r/emacs/comments/ak5pc1/why_does_emacs_keep_asking_me_for_the_right/
(prefer-coding-system 'utf-8)

;; --------------------------------------------------------------------------------
;; * User Interface Setup  ----------------------------

;; Remove welcome message
(setq inhibit-startup-message t)

(tool-bar-mode -1)         ; Disable the toolbar
(tooltip-mode 1)           ; Enable tooltips

;; Settings only for graphical environments
;; not required for terminal mode
(when (display-graphic-p)

	;; Turn off scroll bar, use modeline indicator instead
	(scroll-bar-mode -1)
	;; (scroll-bar-mode 'right)   ; Enable visible scrollbar
	;; Enable mouse context menu (only available in Emacs 28+)
	(context-menu-mode t)

	;; Give some breathing room
	(set-fringe-mode 10)
	)

;; Highlight current line
;; Makes it easier editing long lines, text tables
(global-hl-line-mode 1)

;; Disable the menu bar, press F10 at anytime to access or menu-bar-open
(menu-bar-mode -1)
;; Wrap text in all buffers
(setq-default visual-line-mode t)
(global-visual-line-mode 1)

;; Set up the visible bell, flash on warnings
;; like scrolling to end of buffer repeatedly
(setq visible-bell t)

;; Enable line numbers and customize format
(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
								prog-mode-hook
											conf-mode-hook))
	(add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
	(add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Show matching parentheses, if off screen shown in option specified
;; in variable help - Emacs 29.1
(setq show-paren-context-when-offscreen 'overlay)
;; proceed info from proced shows details in colour, easier to read - Emacs 29.1
(setq proced-enable-color-flag t)

;; --------------------
;; ** isearch settings

;; isearch: Display a counter showing the number of the current and other matches
;; Place it before the prompt
;; from https://protesilaos.com/codelog/2023-06-10-emacs-search-replace-basics/
(setq isearch-lazy-count t
			lazy-count-prefix-format "%s/%s "
			lazy-count-suffix-format nil)

;; Make regular Isearch interpret the empty space as a regular
;; expression that matches any character between the words you give
;; it. Does not affect isearch regex commands
(setq search-whitespace-regexp ".*?")

;; --------------------------------------------------------------------------------
;; * IDE Like Settings  ----------------------------

;; switch-to-buffer-in-dedicated-window: Controls what happens if you attempt to switch buffers in a dedicated window like sidebars
;; - prefer pop to the default to have it pop up the buffer somewhere else than simply error out.
;; switch-to-buffer-obey-display-actions: If nil, the default, user-switched buffers are exempt from display buffer actions set in display-buffer-alist
;; - "You probably want those rules to affect your interactive buffer switching, as it makes for a consistent buffer switching experience."
;; from: https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)

;; --------------------------------------------------------------------------------
;; * Changes adapted from Yay-Evil emacs distro by Ian Y.E. Pan -------------------
;; Licensed under GPL3 from https://github.com/ianyepan/yay-evil-emacs/tree/master
;; Unopinionated and was created for general use
;; Native Emacs packages Section

;; Splitting Windows
;; The Emacs default split doesn't seem too intuitive for most users.
;; Focus on the split window after splitting.
(defun ian/split-and-follow-horizontally ()
  "Split window below."
  (interactive)
  (split-window-below)
  (other-window 1))
(defun ian/split-and-follow-vertically ()
  "Split window right."
  (interactive)
  (split-window-right)
  (other-window 1))
(global-set-key (kbd "C-x 2") #'ian/split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") #'ian/split-and-follow-vertically)

;; Reduce the matching parenthese highlight delay to instantly.
(setq show-paren-delay 0)

;; Auto-pairing quotes and parentheses etc
;; Hook electric-pair-mode to all programming modes
;; (add-hook 'prog-mode-hook 'electric-pair-mode)

;; Clean up whitespace at end of files before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; --------------------------------------------------------------------------------
;; * Themes  ----------------------------

;; Treat all custom themes as safe to load
(setq custom-safe-themes t)

;; Built in theme: tango-dark or wombat
(load-theme 'wombat t)

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

;; Dired tries to guess a default target directory
;; Example, useful with two dired buffers
;; can coping files between them
(setq dired-dwim-target t)
;; Use system's trash can instead of hard delete
(setq delete-by-moving-to-trash t)


;; --------------------------------------------------------------------------------
;; * Org mode ----------------------------

;; Allow shift select to be used outside of shift and cursor sensitive items in org mode
;; 'always restricts shift select to:
;; - Shift cursor is disabled for TODO task changing, bullet changes and properties
;; - Shift curosr  will still change time stamps, see variable's help for information
(setq org-support-shift-select 'always
      ;; Hide bold, italic, code markers
      org-hide-emphasis-markers t)

;; Hide link details and just show description as a link
(setq org-link-descriptive t)

;; --------------------------------------------------------------------------------
;; * Key Bindings ----------------------------

;; Notable defaults
;; - Keyboard start macro: F3
;; - Keyboard end macro: F4

;; -------- Operating System/Common Friendly Key Bindings ------------
;; Complement Common User Access (CUA) Mode bindings from https://wiki.systemcrafters.net/emacs/#transitioning-to-emacs

;; Bind completion-at-point to default
(global-set-key (kbd "C-M-i") 'completion-at-point)

;; Bind mark-whole-buffer aka select all to C-a
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Bind undo to C-z
(global-set-key (kbd "C-z") 'undo)

;; Enable C-c, C-x, C-v copy, cut, paste
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(transient-mark-mode 1) ;; No region when it is not highlighted

;; Other key bindings

;; Make ESC quit prompts (like C-g), useful for vi bindings, global set means active on all modes
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Revert-buffer with F5
(global-set-key (kbd "<f5>") 'revert-buffer)
;; Do not prompt unless file is unsaved or has changes on disk
(setq revert-without-query '(".*"))

;; Find files in a project
(global-set-key (kbd "C-p") 'project-find-file)

;; Browse recently opened files
(global-set-key (kbd "C-r") 'recentf-open-files)

;; --------------------------------------------------------------------------------
;; * General Leader Key F2
;; Buffer Evaluation
;; Bind eval-buffer to F2 e b
(global-set-key (kbd "<f2> e b") 'eval-buffer)
;; Bind eval-region to F2 e r
(global-set-key (kbd "<f2> e r") 'eval-region)

(global-set-key (kbd "<f2> c f") 'jt/copy-file-name-to-clipboard)

(global-set-key (kbd "<f2> k a") 'jt/kill-all-buffers-except-starred)

(global-set-key (kbd "<f2> k t") 'kill-this-buffer)

;; Windows navigation using windmove starting in Emacs 21
;; Overrides defaults which are: S-<arrow keys>
(global-set-key (kbd "<f2> <left>")  'windmove-left)
(global-set-key (kbd "<f2> <right>") 'windmove-right)
(global-set-key (kbd "<f2> <up>")    'windmove-up)
(global-set-key (kbd "<f2> <down>")  'windmove-down)'

;; Buffer Management

;; Bind switch buffer to C-tab
(global-set-key (kbd "<C-tab>") 'switch-to-buffer)

;; Bind kill-this-buffer without prompt to C-w, run kill-region only if no text is marked
;; Binding is similar to CUA-mode where new binding is only active if no region is selected
(global-set-key (kbd "C-w") 'jt/kill-region-or-kill-this-buffer)


;; Bind save-buffer aka save file to C-s
(global-set-key (kbd "C-s") 'save-buffer)

;; Open Terminals
(defun jt/term ()
  "ansi-term bash."
  (interactive)
  (ansi-term "bash")
  ;; Rename term buffer after directory it was launched in like *term* ~/dotfiles
  (rename-buffer (concat (abbreviate-file-name default-directory) " *term*"))
  )

(defun jt/eshell ()
  "eshell."
  (interactive)
  (eshell)
  ;; Rename term buffer after directory it was launched in like *eshell* ~/dotfiles
  (rename-buffer (concat (abbreviate-file-name default-directory) " *eshell*"))
  )

;; Bind C-` to ansi-term bash
(global-set-key (kbd "C-`") 'jt/term)

;; Bind C-~ to eshell
(global-set-key (kbd "C-~") 'jt/eshell)

;; --------------------------------------------------------------------------------
;; * Custom Functions ----------------------------

(defun jt/kill-all-buffers-except-starred ()
  "Kill all buffers except buffers with stars *."
  (interactive)
  (kill-matching-buffers "^[^*]" nil t)
  )

;; Get name of current file without full path and copy to kill ring
(defun jt/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the kill ring (clipboard)."
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))

;; To be used with close window binding
;; similar to CUA-mode where new binding is only active if no region is selected
(defun jt/kill-region-or-kill-this-buffer ()
	"Run kill-region only if no text is marked, else kill-this-buffer."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
		;; kill buffer, ensure only 1 window exists
    (kill-this-buffer)
		(delete-other-windows)
		)
	)

;; --------------------------------------------------------------------------------
;; * Custom Functions from Macro  ----------------------------
;; Placeholder

(provide 'minimal)

;;; minimal.el ends here
