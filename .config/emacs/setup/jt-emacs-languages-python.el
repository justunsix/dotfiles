;;; jt-emacs-languages-python.el --- python and org babel literate programming with python
;;  Author: Justin Tung
;;; Commentary:
;; Configurations for python and literate programming in org babel
;;; Code:

;; --------------------------------------------------------------------------------
;; ** Python-mode

;; from https://github.com/daviwil/emacs-from-scratch/blob/dd9320769f3041ac1edca139496f14abe147d010/Emacs.org#python
;; prerequisites:
;; - run: python3 -m pip install --user "python-lsp-server"
;;   -  or python3 -m pip install install pyright
;; - $HOME/.local/bin in your PATH

;; python.el
;; Default indentation
(setq python-indent-offset 2)

(use-package python-mode
	;; prevent loading of python.el
	:ensure t
	:hook
	(python-mode . lsp-deferred)
	:config
	;; Set location of pyls lsp server otherwise Emacs
	;; cannot find the lsp server
	(setq lsp-pyls-server-command "~/.local/bin/pylsp")
	:custom
	;; Set for python3 by default
	(python-shell-interpreter "python3")
	(when jt/windows-p
		;; set to python for Windows installed by chocolatey
		;; alternatively, python can also work for conda environments
		(setq python-shell-interpreter "python")
		)
	;; Use pytest for unit tests
	;; Debugger
	;; (dap-python-executable "python3")
  ;; (dap-python-debugger 'debugpy)
  ;; :config
  ;; (require 'dap-python)
	)

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
					conda-env-subdirectory "envs")
					;; Load activated conda environment into Emacs or base as default
					;; To populate CONDA_DEFAULT_ENV, activate an env and open Emacs for it to detect it
					(unless (getenv "CONDA_DEFAULT_ENV")
						(conda-env-activate "base"))
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

(provide 'jt-emacs-languages-python)

;;; jt-emacs-languages-python.el ends here
