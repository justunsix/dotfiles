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
(if (file-exists-p (concat (getenv "DOTFILES_OVERLAY_DIR") "/.env"))
		(load (concat (getenv "DOTFILES_OVERLAY_DIR") "/files/.config/emacs/env.el")))

(require 'minimal)
(require 'other)

;; Make gc pauses faster by decreasing the threshold.
;; to avoid Emacs memory consumption over time and
;; avoid large garbage collections
;; Setting to 2MB
;; See also https://akrl.sdf.org/ for further tweaks in runtime
(setq gc-cons-threshold (* 2 1000 1000))
