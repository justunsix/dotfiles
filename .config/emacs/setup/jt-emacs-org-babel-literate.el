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
;; ** Jupyter Integration
;; Setup recommended by:
;; - High level settings and usage: https://michaelneuper.com/posts/replace-jupyter-notebook-with-emacs-org-mode/
;; - Detailed configuration: https://sqrtminusone.xyz/posts/2021-05-01-org-python/
;; Defer after org is loaded
(with-eval-after-load 'org
	(setq
	 org-confirm-babel-evaluate nil)

	;; Always see inline images from output
	(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; When in Windows set conda-home as scoop's miniconda3 install location
	;; When in Linux set conda-home as ""~/miniconda3"
	(setq jt/conda-home "~/miniconda3/")
	(when jt/windows-p
		(setq jt/conda-home "~/scoop/apps/miniconda3/current"))

	;; *** Anaconda / Conda Environments
	(use-package conda
		:config
		(setq conda-anaconda-home (expand-file-name jt/conda-home))
		(setq conda-env-home-directory (expand-file-name jt/conda-home))
		(setq conda-env-subdirectory "envs"))

	;; Load activated conda environment into Emacs or base as default
	;; To populate CONDA_DEFAULT_ENV, activate an env and open Emacs for it to detect it
	(unless (getenv "CONDA_DEFAULT_ENV")
		(conda-env-activate "base"))

	;; Jupyter Language Support
	(use-package jupyter)

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
		 ;; Allow usage of source block of jupyter-LANG, e.g. jupyter-python
		 (jupyter . t)
		 (shell . t)
		 (sql . t)
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
	;; Replaced by yassnippet due to complexity
	;; (add-to-list 'org-structure-template-alist '("plant" . "src plantuml"))

	;; Other
	;;  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
	;;  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
	;;  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
	;;  (add-to-list 'org-structure-template-alist '("go" . "src go"))

	(add-to-list
	 'org-src-lang-modes '("plantuml" . plantuml))

	(setq
	 org-plantuml-jar-path "~/.config/emacs/plantuml/plantuml.jar"
   python-indent-offset 2)


	)

(provide 'jt-emacs-org-babel-literate)

;;; jt-emacs-org-babel-literate.el ends here
