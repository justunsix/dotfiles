;;; jt-emacs-terminal.el --- Personal Emacs terminal configurations
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related to terminals
;;; Code:

;; --------------------------------------------------------------------------------
;; * Terminals ----------------------------

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  ;; to prevent commands from being lost
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for improve  performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  ;(evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  ;(evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  ;(evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        ; Only scroll to bottom on input, output can run and shell stays in same place
        eshell-scroll-to-bottom-on-input t))


(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
	)

(provide 'jt-emacs-terminal)

;;; jt-emacs-terminal.el ends here
