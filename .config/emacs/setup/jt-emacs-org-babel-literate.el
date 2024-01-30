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
;; ** Python and Jupyter Integration

;; Loading in a function so it does not load in start up or background if not required
(defun jt/load-python-jupyter ()
	"Load jupyter integration."
	(interactive)
	;; Setup recommended by:
	;; - High level settings and usage: https://michaelneuper.com/posts/replace-jupyter-notebook-with-emacs-org-mode/
	;; - Detailed configuration: https://sqrtminusone.xyz/posts/2021-05-01-org-python/
	;; See detailed configuration for additional usage and configuration
	;; Defer after org is loaded

	(setq
	 org-confirm-babel-evaluate nil)

	;; Always see inline images after babel execution
	(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

	;; *** Conda configuration
	(setq jt/conda-home "~/.conda/")
	;; When in Windows: set up miniconda3
	;; Set conda-home as scoop's miniconda3 install location
	(when jt/windows-p
		(setq jt/conda-home "~/scoop/apps/miniconda3/current")
		)
	;; *** Anaconda / Conda Environments
	(use-package conda
		:config
		;; Set conda directories and environments
		(setq conda-anaconda-home (expand-file-name jt/conda-home)
					conda-env-home-directory (expand-file-name jt/conda-home)
					conda-env-subdirectory "envs"
					;; Load activated conda environment into Emacs or base as default
					;; To populate CONDA_DEFAULT_ENV, activate an env and open Emacs for it to detect it
					(unless (getenv "CONDA_DEFAULT_ENV")
						(conda-env-activate "base"))
					)
		)

	(use-package jupyter
		:config
		;; *** Workaround ZMQ errors https://github.com/emacs-jupyter/jupyter/issues/464
		;; Retry jupyter connection if problem persists
		(setq jupyter-use-zmq nil)
		;; Do not use native compilation if you get ZMQ subprocess error:
		;; https://github.com/emacs-jupyter/jupyter/issues/297
		;; (setq comp-deferred-compilation-deny-list (list "jupyter"))

		;; Add Jupyter to org-babel-load-languages list
		;; and load it with org-babel-do-load-languages
		(org-babel-do-load-languages 'org-babel-load-languages
																 (append org-babel-load-languages
																				 ;; Allow usage of source block of jupyter-LANG, e.g. jupyter-python
																				 '((jupyter . t)
																					 )
																				 )
																 )
		)

	;; Refresh jupyter kernelspec after an environment switch
	(defun my/jupyter-refresh-kernelspecs ()
		"Refresh Jupyter kernelspecs"
		(interactive)
		(jupyter-available-kernelspecs t))

	)

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
