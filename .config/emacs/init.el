;;; init.el ---
;;; Author: Justin Tung
;;; Commentary:
;; Load personal variables and packages and adjust garbage collection on startup
;;; Code:
;; Add emacs user directory to load-path so that we can load files to emacs
(add-to-list 'load-path (concat user-emacs-directory "setup/"))

;; Adjust garbage collection to higher during start up
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Variables used in Emacs configuration below, override with your settings.
;; File is located in ~/.config/emacs/setup/variables.el
(require 'variables)

;; --------------------------------------------------------------------------------
;; * Overlay  ----------------------------
;; Optionally load dotfiles overlay Emacs configurations
(setq env-el-location "~/.config/emacs/setup/env.el")
(if (file-exists-p env-el-location)
		(load env-el-location)
	)

;; --------------------------------------------------------------------------------
;; Load native emacs configurations
(require 'minimal)

;; --------------------------------------------------------------------------------
;; Load 3rd party packages and their configurations
(require 'other)
;; user interface
(require 'jt-emacs-ui)
;; org-mode related
(require 'jt-emacs-org)
;; org-babel
(require 'jt-emacs-org-babel-literate)
;; org-roam related
(require 'jt-emacs-org-roam)
;; artificial intelligence (AI)
(require 'jt-emacs-ai)
;; programming languages
(require 'jt-emacs-languages)
;; terminal, shell
(require 'jt-emacs-terminal)
;; spelling of words
(require 'jt-emacs-spelling)
;; completion frameworks
(require 'jt-emacs-completion-ivy-counsel)
;; directory and files
(require 'jt-emacs-dired)


;; --------------------------------------------------------------------------------
;; Post Emacs Start GC

;; Make gc pauses faster by decreasing the threshold.
;; to avoid Emacs memory consumption over time and
;; avoid large garbage collections
;; Setting to 2MB
;; See also https://akrl.sdf.org/ for further tweaks in runtime
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
