;;; jt-emacs-package-managers.el --- Emacs Package managers
;;  Author: Justin Tung
;;; Commentary:
;; Emacs configuration of package managers
;;; Code:

;; --------------------------------------------------------------------------------
;; * Package Manager Configuration ----------------------------

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Allow imenu to find use-package and require forms
;; Must be enabled before require 'use-package
(setq use-package-enable-imenu-support t)

(require 'use-package)

;; Emacs 29.1: If you want to be able to use 'package-install' to upgrade use-package
;; to newer versions released on GNU ELPA, customize the new option
;; 'package-install-upgrade-built-in' to a non-nil value.

;; All use-package will automatically install the package
(setq use-package-always-ensure t
			;; Always defer packages unless explicitly demanded on startup
			;; In case of errors, force loading on packages with `demand t`
			user-package-always-defer t
			;; If verbose useful for debugging use-package loading
			use-package-verbose nil)

;; --------------------------------------------------------------------------------
;; * Straight Package Management ----------------------------

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

;; If the day is Sunday, run (straight-pull-all)
;; Only run if want to grab updates
;; (when (equal (nth 6 (decode-time)) 0)
;;  (straight-pull-all))

;; Use straight.el for use-package expressions
;; (straight-use-package 'use-package)

;; --------------------------------------------------------------------------------
;; * Package Updates ----------------------------

;; Configure packages to automatically update
(use-package auto-package-update
  ;; Defer 20 seconds after startup
  :defer 20
  :custom
  ;; Interval in days
  (auto-package-update-interval 30)
  ;; Ask before auto update
  (auto-package-update-prompt-before-update t)
  ;; Show results, default is nil
  (auto-package-update-hide-results nil)
  :config
  ;; Check if interval passed
  (auto-package-update-maybe)
  ;; 2nd check in case you rarely restart Emacs
  ;; Around 7pm, check for updates
  ;; (auto-package-update-at-time "19:20"))
	;; Trigger explicitly `M-x auto-package-update-now` to update

	;; Delete old versions after updates
	(setq auto-package-update-delete-old-versions t)
)

(provide 'jt-emacs-package-managers)

;;; jt-emacs-package-managers.el ends here