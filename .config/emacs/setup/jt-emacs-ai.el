;; jt-emacs-ai.el --- Personal artificial intelligence, copilot configurations and related packages
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related to artificial intelligence
;;; Code:

;; --------------------------------------------------------------------------------
;; * Copilot ----------------------------

(defun jt/copilot ()
	"Activate copilot only under certain conditions."
  (copilot-mode 1)
  )

;; Installation and instructions at https://github.com/zerolfx/copilot.el
;; Set copilot-node-executable to environment variable NVM_BIN managed by nvm
;; (setq copilot-node-executable (concat (getenv "NVM_BIN") "/node"))

;; Manually set node executable as NVM_BIN is not set unless launched from shell
(setq copilot-node-executable
      (car (directory-files-recursively "~/.nvm/versions/node/" "node$" nil t)))

;; if ~/.nix-profile/bin/node exists, use it instead
(when (file-exists-p "~/.nix-profile/bin/node")
  (setq copilot-node-executable "~/.nix-profile/bin/node"))

(when jt/windows-p
  ;; Set to nodejs location installed by scoop in user's folder
  ;; Previous configuration was set to nodejs v17 folder structure using Windows 64 bit binary from https://nodejs.org/download/release/v17.9.1/ as if it was installed by nvm
  (setq copilot-node-executable "~/scoop/apps/nodejs/current/node.exe")
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

;; Suppress warnings when a buffer exceeds the variable copilot-max-char
(require 'warnings)
;;;; suggested at https://github.com/zerolfx/copilot.el/pull/198
;;;; and configured per https://github.com/blahgeek/emacs.d/blob/master/init.el
(setq warning-suppress-types
        '((emacs)
          (copilot copilot-exceeds-max-char)
					(copilot copilot-no-mode-indent))
				)
;;;; Having issues with using add-to-list to add to warning-suppress-types for some reason
;;;; (add-to-list 'warning-suppress-types '((emacs)
;;;;                 (copilot copilot-exceeds-max-char)))

;; Set copilot-overlay-face to inherit from font-lock-comment-face
;; Default is `shadow` which is too light
(set-face-attribute 'copilot-overlay-face nil
                    :inherit 'font-lock-comment-face)

(provide 'jt-emacs-ai)

;;; jt-emacs-ai.el ends here
