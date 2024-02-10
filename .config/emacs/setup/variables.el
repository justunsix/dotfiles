;; --------------------------------------------------------------------------------
;; * Variables  ----------------------------
;; Used in Emacs configuration files
;; Override with your settings

;; Location of your org files
(defvar jt/org-directory (concat (getenv "HOME") "/org"))

;; Directory containing projects, can be list of paths like ("~/projects" "~/work")
(defvar jt/project-search-path (concat (getenv "HOME") "/Code"))

;; Location of dotfiles
(defvar jt/dotfiles-dir (concat (getenv "HOME") "/Code/dotfiles"))

;; Location of your bongo playlist file (optional)
(defvar jt/bongo-playlist-location (concat (getenv "HOME") "/Music/bongoplaylist"))

(provide 'variables)
