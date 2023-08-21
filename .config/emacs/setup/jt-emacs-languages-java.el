;;; jt-emacs-languages-java.el --- Java programming configurations
;;  Author: Justin Tung
;;; Commentary:
;; Configurations for Java programming
;;; Code:

;; --------------------------------------------------------------------------------
;; ** Java Packages

;; Loading in a function so it does not load in start up or background if not required
(defun jt/load-java()

	;; --------------------------------------------------------------------------------
	;; Java
	;; from https://talks.skybert.net/emacs-java-setup/emacs-java.html
	;;  Install lsp-java & dap-java

	(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
		:config (setq lsp-completion-enable-additional-text-edit nil))
	(use-package hydra)
	(use-package company)
	(use-package lsp-ui)
	(use-package which-key :config (which-key-mode))
	(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
	(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
	(use-package dap-java :ensure nil)
	(use-package helm-lsp)
	(use-package helm
		:config (helm-mode))
	(use-package lsp-treemacs)

	;; Compilation escape codes
	(add-hook 'compilation-filter-hook
						(lambda () (ansi-color-apply-on-region (point-min) (point-max))))

	;; IDEA style shortcuts
	(use-package lsp-mode
    :bind
    (:map lsp-mode-map
          (("\C-\M-b" . lsp-find-implementation)
           ("M-RET" . lsp-execute-code-action))))

	(use-package dap-java
		:ensure nil
		:after (lsp-java)

		:config
		(global-set-key (kbd "<f7>") 'dap-step-in)
		(global-set-key (kbd "<f8>") 'dap-next)
		(global-set-key (kbd "<f9>") 'dap-continue))

	;; Java mode hook
	(defun my-java-mode-hook ()
		(auto-fill-mode)
		(flycheck-mode)
		(git-gutter+-mode)
		(subword-mode)
		(yas-minor-mode)
		(set-fringe-style '(8 . 0))
		(define-key c-mode-base-map (kbd "C-M-j") 'tkj-insert-serial-version-uuid)
		(define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
		(define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)

		;; Fix indentation for anonymous classes
		(c-set-offset 'substatement-open 0)
		(if (assoc 'inexpr-class c-offsets-alist)
				(c-set-offset 'inexpr-class 0))

		;; Indent arguments on the next line as indented body.
		(c-set-offset 'arglist-intro '++))
	(add-hook 'java-mode-hook 'my-java-mode-hook)

	)

(provide 'jt-emacs-languages-java)

;;; jt-emacs-languages-java.el ends here
