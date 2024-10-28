;; from modules/jt/doomconfig/autoload.el
(defvar jt-org-presentation-mode nil
  "Non-nil if presentation mode is enabled.")

;;;###autoload
(defun jt/org-toggle-presentation-mode ()
  "Toggle presentation mode settings."
  (interactive)
  (if jt-org-presentation-mode
      (progn
        ;; Revert to default settings
        (setq doom-font (font-spec :family "Jetbrains Mono" :size 15))
        (load-theme 'doom-tokyo-night t)
        (display-line-numbers-mode 1)
        (doom/reload-font)        
        (setq jt-org-presentation-mode nil)
        (message "Presentation mode disabled"))
    (progn
      ;; Apply presentation settings
      (setq doom-font (font-spec :family "Jetbrains Mono" :size 15))
      (load-theme 'doom-plain t)
      (display-line-numbers-mode -1)
      (doom/reload-font)
      (setq jt-org-presentation-mode t)
      (message "Presentation mode enabled"))))
