;;; jt-emacs-org.el --- Personal org-mode related configurations and packages
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related to org-mode
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
							 ("C-c C-o" . jt/org-open-at-point)
							 ("C-b" . 'jt/org-boldify-region)
							 )
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
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
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
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
	;; Hashicorp Configuration Language (HCL)
  (add-to-list 'org-structure-template-alist '("hcl" . "src hcl"))
	(add-to-list 'org-structure-template-alist '("ps1" . "src powershell"))
	;; Replaced by yassnippet due to complexity
  ;; (add-to-list 'org-structure-template-alist '("plant" . "src plantuml"))

	;; Other
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
;; * Key Bindings ----------------------------

;; Bind org-toggle-link-display to F2 l
(global-set-key (kbd "<f2> l") 'org-toggle-link-display)

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

(defun jt/org-boldify-region ()
  "Boldify the selected region in Org mode or unbold if already."
  (interactive)
  (when (region-active-p)
    (org-emphasize ?*)
		)
	)

(provide 'jt-emacs-org)

;;; jt-emacs-org.el ends here
