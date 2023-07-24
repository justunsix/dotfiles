;;; jt-emacs-org-roam.el --- org-mode roam related configurations and packages
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related to org-roam
;;; Code:

;; --------------------------------------------------------------------------------
;; * Org-roam Setup --------------------------

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
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
				;; Configure what sections are displayed in the org-roam buffer
				;; backlinks, notes referencing it, and nodes that contain text that match the node's title/alias
				org-roam-mode-sections
				(list #'org-roam-backlinks-section
							#'org-roam-reflinks-section
							;; #'org-roam-unlinked-references-section
							)
				)

  ;; Sync org-roam database
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;;  (require 'org-roam-protocol)
  )

;; Set how org-roam buffer should be displayed
;; See other configurations and permanent side window  at:
;; https://www.orgroam.com/manual.html#Navigating-the-Org_002droam-Buffer
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.23)
               (window-height . fit-window-to-buffer)))

;; Disable doing org-roam database sync on startup on Windows
;; due to query timeouts, ok to run manually after startup
(unless jt/windows-p

  ;; Synchronize database
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

(provide 'jt-emacs-org-roam)

;;; jt-emacs-org-roam.el ends here
