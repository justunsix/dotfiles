;;; jt-emacs-org.el --- Personal org-mode related configurations and packages
;;;  Author: Justin Tung
;;; Commentary:
;;; Configurations related to org-mode
;;; Code:

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
	;; Begin org config

  ;; Debug message to check when org mode is loading
  ;; (message "Loading Org Mode...")

  (setq
	 ;; Instead of ellipsis in headings, use this value
	 org-ellipsis " ▾"
	 ;; Fontify code blocks
	 org-src-fontify-natively t
	 ;; Show inline images on startup
	 org-startup-with-inline-images t
	 )
  (efs/org-font-setup)

	;; Add where are your org files
	(setq org-directory jt/org-directory
				org-agenda-files (list org-directory)
				;; Set org-agenda-file-regexp as a regular expression to  match all .org files
				;; with the word task in the filename
				;; original value was: "\\`[^.].*\\.org\\'"
				;; Filter using only org files with the name task in them
				org-agenda-file-regexp "\\`[^.].*Task.*\\.org\\'"
				;; Default note for org-capture
				org-default-notes-file (expand-file-name "Journal.org" org-directory)
				org-agenda-start-with-log-mode t
				;; When task is done, log time
				org-log-done 'time
				org-log-into-drawer t
				;; Prepare for image resizing
				org-image-actual-width t
				)

	(setq org-todo-keywords
				'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
					(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

	;; Set org priorities ranging from A to J
	(setq  org-enable-priority-commands t
				 org-highest-priority ?A
				 org-default-priority ?J
				 org-lowest-priority ?J
				 )

	;; end of org config
  :bind (("C-c a" . org-agenda)
         ;; Bind C-c l to my-org-insert-link similar to org-insert-link
         ("C-c l" . my-org-insert-link)
				 ;; Bind org-insert-heading
				 ;; changed to this keybinding to avoid conflicts with jupyter packages
				 ("C-c C-h" . org-insert-heading)
         ;; Override org-mode's org-open-at-point with jt/org-open-at-point
				 ;; This allows for opening links in the same window
				 (:map org-mode-map
							 ("C-c C-o" . jt/org-open-at-point)
							 ("C-b" . 'jt/org-boldify-region)
							 ;; ace-link package keybinding
							 ("M-o" . ace-link-org)
							 )
				 )
	)

;; Configure org font faces
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
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  ;; Increase weight of bold text in org-mode for readability
  (set-face-attribute 'bold nil :weight 'ultra-bold)
	)

;; Replace headings with different types of bullets
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

;; --------------------------------------------------------------------------------
;; * Org Export to HTML --------------------------

;; Ensure htmlize is installed
;; otherwise errors like "Cannot fontify source block (htmlize.el >= 1.34 required)" will occur
(use-package htmlize
	:after org
	:config
	;; html output using css instead of inline styles
	;; per https://github.com/gongzhitaao/orgcss
	;; recommend use in org files to export
	;; #+HTML_HEAD: <link rel="stylesheet" type="text/css" href="https://gongzhitaao.org/orgcss/org.css"/>
	(setq org-html-htmlize-output-type 'css
				org-html-head-include-default-style nil)
	)

;;  (use-package org-preview-html
;;    :after org
;;    )

;; --------------------------------------------------------------------------------
;; * Org Links --------------------------

(defun jt-org-insert-link ()
  "Insert org link where description is set to html title."
  (interactive)
  (let* ((url (read-string "URL: "))
         (title (www-get-page-title url)))
    (org-insert-link nil url title)))

;; Re: How to get title of web page by url?
;; from https://lists.gnu.org/archive/html/help-gnu-emacs/2010-07/msg00291.html
(defun www-get-page-title (url)
  (let ((title))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
      (setq title (match-string 1))
      (goto-char (point-min))
      (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
      (decode-coding-string title (intern (match-string 1))))))

;; Solution from Org mode hacks, not used due to needing mm-url
;;
;; https://orgmode.org/worg/org-hacks.html
;; Insert link with HTML title as default description
;;  When using `org-insert-link' (`C-c C-l') it might be useful to extract contents from HTML <title> tag and use it as a default link description.
;; (require 'mm-url) ; to include mm-url-decode-entities-string
;; (defun get-html-title-from-url (url)
;;   "Return content in <title> tag."
;;   (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
;;     (save-excursion
;;       (set-buffer download-buffer)
;;       (beginning-of-buffer)
;;       (setq x1 (search-forward "<title>"))
;;       (search-forward "</title>")
;;       (setq x2 (search-backward "<"))
;;       (mm-url-decode-entities-string (buffer-substring-no-properties x1 x2)))))


;; --------------------------------------------------------------------------------
;; * Images in Org  ----------------------------

;; Moving images from point A (external, clipboard, web, etc. to point B (org file, directory, etc.))
;; Favour using (org-download-clipboard) to get images or org-mode attach for links

(use-package org-download
	:after org
	:config
	;; Drag-and-drop to `dired`
	(add-hook 'dired-mode-hook 'org-download-enable)
	:custom
	(org-download-method 'directory)
	;; org-download will store images in <org-directory>/../media
	(org-download-image-dir (expand-file-name "../media" org-directory))
	(org-download-heading-lvl nil)
	(org-download-timestamp "%Y%m%d-%H%M%S_")
	)

;; --------------------------------------------------------------------------------
;; * Key Bindings ----------------------------

;; Bind org-toggle-link-display to F2 l
(global-set-key (kbd "<f2> l") 'org-toggle-link-display)
;; Recommend to use org capture from anywhere in Emacs
;; per org manual: https://orgmode.org/manual/Activation.html and
;; https://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(global-set-key (kbd "C-c c") #'org-capture)

;; --------------------------------------------------------------------------------
;; * Org Capture Templates ----------------------------

;; See parts of a template at:
;; https://orgmode.org/manual/Template-elements.html

(setq org-capture-templates
      '(("n" "New Note with Org ID" plain
				 ;; Use a function to set the filename
         (file jt/org-capture-create-filename)
				 "#+title: %^{Title}%?\n#+filetags: %^g \n")
				)
			)

;; (setq org-capture-templates
;;       '(("n" "New Note with Org ID" entry
;;          (file jt/org-capture-create-filename)
;; 				 "%(format \"#+TITLE: %s\n#+STAMP: %s\n\" my-org-note--name my-org-note--time)"
;; 				 ))
;; 			)

;; Inspired from https://emacs.stackexchange.com/a/40933
(defun jt/org-capture-create-filename ()
	"Set a filename for a new note to be used in an 'org-capture-template'."
	(setq jt/org-note-name (read-string "Enter new filename (without extension): "))
  (setq jt/org-note-time (format-time-string "%Y%m%d%H%M%S"))
  (expand-file-name (format "%s.org" jt/org-note-name) org-directory)
	)

;; --------------------------------------------------------------------------------
;; * Custom Functions ----------------------------

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

(defun jt/create-org-note (title)
  "Create a new org file in current directory, add an ID, prompt user for
	title, filetags. Filename will be the title with spaces replaced by hyphens."
  (interactive "sEnter the title of the org file: ")
  (let*
	((filename (concat (replace-regexp-in-string " " "-" title) ".org"))
         (buffer (find-file filename))
         (tags (read-string "Enter tags separated by colon: "))
				 (id-string (concat ":PROPERTIES:\n:ID: " (org-id-get-create) "\n:END:\n"))
         (title-string (concat "#+title: " title "\n"))
         (tags-string (concat "#+filetags: " tags "\n\n* See Also\n\n"))
				 (file-link-string (concat "- [[file:" filename "][" title "]] - "))
				 )
    (with-current-buffer buffer
      (insert id-string)
      (insert title-string)
      (insert tags-string)
			(insert file-link-string)
			)
    (save-buffer)
		)
	)

(defun jt/org-boldify-region ()
  "Boldify the selected region in Org mode or unbold if already."
  (interactive)
  (when (region-active-p)
    (org-emphasize ?*)
		)
	)

(provide 'jt-emacs-org)

;;; jt-emacs-org.el ends here
