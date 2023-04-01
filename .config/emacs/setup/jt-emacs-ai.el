;;; jt-emacs-ai.el --- Personal artificial intelligence, copilot configurations and related packages
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related to artificial intelligence
;;; Code:

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

;; Use straight.el for use-package expressions
;; (straight-use-package 'use-package)

;; --------------------------------------------------------------------------------
;; * Copilot ----------------------------

(defun jt/copilot ()
	"Activate copilot only under certain conditions."
  (copilot-mode 1)
  )

;; Installation and instructions at https://github.com/zerolfx/copilot.el
(setq copilot-node-executable "~/.nvm/versions/node/v18.14.0/bin/node")

(when jt/windows-p
  ;; Set to nodejs location installed by Chocolately
  ;; Previous configuration was set to nodejs v17 folder structure using Windows 64 bit binary from https://nodejs.org/download/release/v17.9.1/ as if it was installed by nvm
  (setq copilot-node-executable "C:/Program Files/nodejs/node.exe")
  )

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

(provide 'jt-emacs-ai)

;;; jt-emacs-ai.el ends here
