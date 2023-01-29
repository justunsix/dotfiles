;; Minimal configuration that works with default Emacs 28.2+ with no extra packages

;; --------------------------------------------------------------------------------
;; * Emacs General Settings -------------------------------------------------------

;; Change yes or no to y or n to simplify typing
(fset 'yes-or-no-p 'y-or-n-p)

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

;; --------------------------------------------------------------------------------
;; * User Interface Setup  ----------------------------

;; Remove welcome message
(setq inhibit-startup-message t)

(tool-bar-mode -1)         ; Disable the toolbar
(tooltip-mode 1)           ; Enable tooltips

;; Settings only for graphical environments
;; not required for terminal mode
(when (display-graphic-p)
	(scroll-bar-mode 'right)   ; Enable visible scrollbar
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

;; --------------------------------------------------------------------------------
;; * IDE Like Settings  ----------------------------

;; Force Emacs to place shell and terminals at the bottom, with a window height of no more than 30% of the size of the frame.
;; From: https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(add-to-list 'display-buffer-alist
  '("\\*e?shell\\*" display-buffer-in-direction
    (direction . bottom)
    (window . root)
    (window-height . 0.3))
	)

;; switch-to-buffer-in-dedicated-window: Controls what happens if you attempt to switch buffers in a dedicated window like sidebars
;; - prefer pop to the default to have it pop up the buffer somewhere else than simply error out. 
;; switch-to-buffer-obey-display-actions: If nil, the default, user-switched buffers are exempt from display buffer actions set in display-buffer-alist
;; - "You probably want those rules to affect your interactive buffer switching, as it makes for a consistent buffer switching experience."
;; from: https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)

;; Force Emacs to place terminmals at the bottom, with a window height of no more than 30% of the size of the frame.
(add-to-list 'display-buffer-alist
	'("term\\*$" display-buffer-in-direction
		(direction . bottom)
		(window . root)
		(window-height . 0.3))
	)

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

(global-set-key (kbd "<f2> k") 'jt/kill-all-buffers)

;; Windows navigation using windmove starting in Emacs 21
;; Overrides defaults which are: S-<arrow keys>
(global-set-key (kbd "<f2> <left>")  'windmove-left)
(global-set-key (kbd "<f2> <right>") 'windmove-right)
(global-set-key (kbd "<f2> <up>")    'windmove-up)
(global-set-key (kbd "<f2> <down>")  'windmove-down)

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
	
(defun jt/kill-all-buffers ()
  "Kill all buffers except *scratch*, *Messages* and switch to *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (mapc 'kill-buffer (delq (current-buffer) (delq (get-buffer "*Messages*") (delq  (buffer-list)))))
  ;; Send minibuffer message
  (sleep-for 1)    
  (message "All buffers killed except *scratch*, *Messages*")
  )

(defun jt/insert-date ()
  "Insert the current date in format YYYY-MM-DD into the current buffer"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun jt/insert-date-time ()
  "Insert the current date in format YYYY-MM-DD HHMM into the current buffer"
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H%M")))

;; Get name of current file without full path and copy to kill ring
(defun jt/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
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

