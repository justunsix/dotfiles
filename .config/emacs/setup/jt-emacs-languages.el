;;; jt-emacs-ui.el --- Personal Programming language Support configurations and packages
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related to programming language support
;;; Code:

;; --------------------------------------------------------------------------------
;; * Language and Language Servers ----------------------------

; Breadcrumb to show structure of file and where you are located
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
	)

;; lsp and lsp-deferred will activate lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
	;; set personal lsp-mode feature prefix to overwrite default Super key + L
	;; Or 'C-l', 's-l'
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; yasnippet for lsp and snippet management
(use-package yasnippet
	:hook (lsp-mode . yas-minor-mode)
	:config
	;; Activate yasnippet globally
	;; can be done on per buffer with yas-minor-mode
	(yas-global-mode 1)
	)

;; Install snippet collection
(use-package yasnippet-snippets
	:after yasnippet
	:config
	;; configure personal snippets with concat user emacs directory + snippets
	(setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
	;; Add (yasnippet backquote-change) to ‘warning-suppress-types’
	;; to turn off warnings of elisp executions in snippets
	(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
	)

;; UI enhancements for LSP
;; Can use sideline configuration for more frame help on screen
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
	)

;; Integrate Ivy with lsp-mode to search in code, like symbols
(use-package lsp-ivy
  ;; Defer to after lsp is loaded
  :after lsp)

;; TypeScript mode is activated when .ts file is opened
(use-package typescript-mode
  :mode "\\.ts\\'"
  ; Activate lsp-deferred, deferred so LSP activates only when .ts file is active buffer
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
	)

;; Defer LSP to start later, activate visual line (word wrap)
;; and remove line numbers
(defun efs/markdown-mode-setup ()
	(lsp-deferred)
	(visual-line-mode 1)
	(setq display-line-numbers nil)
	;; Spell check
	(flyspell-mode 1)

	(jt/copilot)
	)
;; Markdown support, syntax highlighting
(use-package markdown-mode
	:ensure t
	:hook (markdown-mode . efs/markdown-mode-setup)
	:mode ("README\\.md\\'" . gfm-mode)
	:init (setq markdown-command "multimarkdown"
							markdown-fontify-code-blocks-natively t)
	:config
	(require 'lsp-marksman)
	)

(use-package yaml-mode
  :hook (yaml-mode)
  :mode "\\.y[a]?ml\\'"
	)

(use-package plantuml-mode
	:mode "\\.plantuml\\'"
	:config
	(setq plantuml-executable-path "/usr/bin/plantuml"
				plantuml-default-exec-mode 'executable
				plantuml-output-type "png"
				)
	)

(use-package json-mode
  :mode "\\.json\\'"
  :hook (json-mode . lsp-mode))

(use-package json-reformat
  :after json-mode
  :bind (:map json-mode-map
         ("C-c C-f" . json-reformat-region)))

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . lsp-mode))

(use-package hcl-mode
	:mode "\\.hcl\\'")

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2 ; HTML
				web-mode-css-indent-offset 2    ; CSS
				web-mode-code-indent-offset 2   ; JS/SX/TS/TSX
				web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
				)
	)

(use-package powershell)
;;	:mode "\\.ps1\\'")

(provide 'jt-emacs-languages)

;;; jt-emacs-languages.el ends here
