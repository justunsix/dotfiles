;;; jt-emacs-dired.el --- Personal dired related configurations and packages
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related to dired
;;; Code:

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

(provide 'jt-emacs-dired)

;;; jt-emacs-dired.el ends here
