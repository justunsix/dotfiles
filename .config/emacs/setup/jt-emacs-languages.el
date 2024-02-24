;;; jt-emacs-ui.el --- Personal Programming language Support configurations and packages
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related to programming language support
;;; Code:

;; --------------------------------------------------------------------------------
;; * Language and Language Servers ----------------------------

; Breadcrumb to show structure of file and where you are located
(defun efs/lsp-mode-setup ()
	"Setup for lsp mode."
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
	;; or M-x yas-reload-all if you've started YASnippet already.
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
	"Setup for markdown mode."
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
	(setq
	 plantuml-output-type "png"
	 ;; Plantuml executable config
	 ;; from https://github.com/skuro/plantuml-mode#execution-modes
	 plantuml-jar-path "~/.config/emacs/plantuml/plantuml.jar"
	 plantuml-default-exec-mode 'jar
	 ;; Alternative executable using binary
	 ;; plantuml-executable-path "/usr/bin/plantuml"
	 ;; plantuml-default-exec-mode 'executable
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

;; csv
;; Recommendations from https://www.emacswiki.org/emacs/CsvMode
;; updated to Emacs 29.1

(require 'color)

(defun csv-highlight (&optional separator)
	"Highlight CSV columns using with different colors.
SEPARATOR is the character to use as a separator."
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (font-lock-mode 1)
  (let* ((separator (or separator ?\,))
         (n (count-matches (string separator) (line-beginning-position) (line-end-position)))
         (colors (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
													collect (apply #'color-rgb-to-hex
																				 (color-hsl-to-rgb i 0.3 0.5)))))
    (cl-loop for i from 2 to n by 2
						 for c in colors
						 for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
          do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))

(use-package csv-mode
	:mode "\\.csv\\'"
	:config
	(add-hook 'csv-mode-hook 'csv-highlight)
	;; csv-align-mode helps with aligning columns on a set width
  ;; (add-hook 'csv-mode-hook 'csv-align-mode)
	;; truncate long lines in a buffer
	;; (add-hook 'csv-mode-hook '(lambda () (interactive) (toggle-truncate-lines nil))))
	)

(use-package rust-mode
	:config
	;; Use spaces instead of tabs per Rust style guide
	(add-hook 'rust-mode-hook
						(lambda () (setq indent-tabs-mode nil)))
	;; Format on save using rustfmt if installed
	(setq rust-format-on-save t)
	(add-hook 'rust-mode-hook #'lsp)
	)

(use-package nix-mode
  :mode "\\.nix\\'")

(provide 'jt-emacs-languages)

;;; jt-emacs-languages.el ends here
