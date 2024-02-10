;;; jt-emacs-org-babel-literate.el --- org babel literate programming configurations
;;  Author: Justin Tung
;;; Commentary:
;; Configurations for literate programming with org babel
;;; Code:

;; --------------------------------------------------------------------------------
;; * Org-babel and Settings  ---------------------------

;; --------------------------------------------------------------------------------
;; ** Tangle Set up

;; Automatically tangle our literate org dotfiles when we save them
(defun efs/org-babel-tangle-config ()
  ;; Check if current buffer-file-name string contains word "Dotfile"
  ;; If it contains "Dotfile", run tangle with no warning prompt
  (when (string-match-p "Dotfile" (buffer-file-name))
		(let ((org-confirm-babel-evaluate nil))
			(org-babel-tangle))
    )
  )
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

;; --------------------------------------------------------------------------------
;; ** org-babel languages

;; Defer after org is loaded
(with-eval-after-load 'org

	;; Configure Babel recognized languages:
	;; elips, python, shell (includes bash)
	;; See list https://orgmode.org/worg/org-contrib/babel/languages/index.html
	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((emacs-lisp . t)
		 (python . t)
		 (shell . t)
		 (sql . t)
		 (makefile . t)
		 ;; plantuml
		 ;; per https://plantuml.com/emacs
		 (plantuml . t)
		 ;; (haskell . t)
		 ;;	(powershell . t)
		 ;; (R . t)
		 )
	 )
	(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

	;; Structure Templates
	;; For org-babel source, templates for langauge e.g. <el + TAB starts template... >
	;; This is needed as of Org 9.2
	(require 'org-tempo)

	(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
	(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
	(add-to-list 'org-structure-template-alist '("py" . "src python"))
	(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
	(add-to-list 'org-structure-template-alist '("txt" . "src txt"))
	(add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
	(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
	(add-to-list 'org-structure-template-alist '("json" . "src json"))
	(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
	;; Hashicorp Configuration Language (HCL)
	(add-to-list 'org-structure-template-alist '("hcl" . "src hcl"))
	(add-to-list 'org-structure-template-alist '("ps1" . "src powershell"))
	;; Replaced by yassnippet due to complexity in the template
	;; (add-to-list 'org-structure-template-alist '("plant" . "src plantuml"))

	;; Other
	;;  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
	;;  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
	;;  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
	;;  (add-to-list 'org-structure-template-alist '("go" . "src go"))

	;; ** PlantUML config
	(add-to-list
	 'org-src-lang-modes '("plantuml" . plantuml))
	(setq
	 org-plantuml-jar-path "~/.config/emacs/plantuml/plantuml.jar")

	)

(provide 'jt-emacs-org-babel-literate)

;;; jt-emacs-org-babel-literate.el ends here
