;;; jt-emacs-org-present.el --- Personal org-mode presentation configurations
;;  Author: Justin Tung
;;; Commentary:
;; Configurations for presentations in org-mode
;;; Code:

;; --------------------------------------------------------------------------------
;; * Org Present --------------------------

(defun jt/load-org-present ()
	"Load org presentation packages."
	(interactive)

  ;; Package to centre text
  (use-package visual-fill-column)
  (use-package org-present)

  ;; Visual-fill-column variables are buffer local variables
  ;; and must set with defaults to apply globally
  (setq-default visual-fill-column-width 220)
  (setq-default visual-fill-column-center-text t)

  ;; Center text on screen in org-present
  (defun efs/org-present-start ()

    ;; Turn off centaur-tabs locally in buffer
    (centaur-tabs-local-mode t)

    ;; Centre presentation and wrap lines
    (visual-fill-column-mode 1)
    (visual-line-mode 1)
    ;; Larger font sizes for presentations
    ;; Relative to existing font heights
    (setq-local face-remapping-alist '((default (:height 1.6) variable-pitch)
                                       (header-line (:height 2.0) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))

    ;; Ensure images are displayed in presentation
    (org-toggle-inline-images)

    ;; Set a blank header line string to create blank space at the top
    (setq header-line-format " ")

    ;; Hide unneeded UI elements (this can even be done in my/org-present-start!)
    (menu-bar-mode 0)
    (tool-bar-mode -1)
    (scroll-bar-mode 0)
    ;; Let the desktop background show through
    ;; (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
    ;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

    ;; Adjust font for presentation friendly fonts
    ;; (set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 120)
    ;; (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 120)
    ;; (set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :weight 'light :height 1.3)

    (set-face-attribute 'default nil :font "Iosevka" :weight 'light :height 120)
    (set-face-attribute 'fixed-pitch nil :font "Iosevka" :weight 'light :height 120)
    (set-face-attribute 'variable-pitch nil :font "Iosevka" :weight 'light :height 1.3)

    ;; Dark Theme for presentations
    ;; (load-theme 'doom-palenight t)

    ;; Light Theme for presentations
    (load-theme 'doom-tomorrow-day t)

    ;; Disable mode line just for presentation buffer
    (setq mode-line-format nil)

    )

  (defun efs/org-present-stop ()

    ;; Re-enable centaur-tabs locally in buffer
    (centaur-tabs-local-mode -1)

    ;; Stop document centre justification
    (visual-fill-column-mode 0)
    ;; Reset font changes
    (setq-local face-remapping-alist '((default variable-pitch default)))
    ;; Clear the header line format by setting to `nil'
    (setq header-line-format nil)
    ;; Turn  UI elements back on
    (menu-bar-mode t)
    (tool-bar-mode 0)
    (scroll-bar-mode t)

    ;; Reset fonts to defaults
    (jt/set-default-fonts)
    ;; Go back to regular theme
    (load-theme 'doom-tokyo-night t)
    )

  ;; Prepare slides with multiple headings to collapse them by default
  ;; Open them during the presentation
  (defun efs/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)

    ;; Unfold the current entry
    (org-show-entry)

    ;; Show only direct subheadings of the slide but don't expand them
    (org-show-children)
    )

  ;; Register hooks with org-present so start and stop configuration
  ;; functions run
  (add-hook 'org-present-mode-hook 'efs/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'efs/org-present-stop)

  ;; Prepare slides by collapsing headings
  ;;(add-hook 'org-present-after-navigate-functions 'efs/org-present-prepare-slide)
  ;; Remove hook for now
  (setq org-present-after-navigate-functions nil)

  ) ; end of jt/load-org-present

(provide 'jt-emacs-org-present)

;;; jt-emacs-org-present.el ends here
