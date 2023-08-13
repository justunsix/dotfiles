;;; jt-emacs-completion-ivy-counsel.el --- Personal completion packages and configurations
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related to completion frameworks
;;; Code:

;; --------------------------------------------------------------------------------
;; * Counsel ----------------------------
;; Navigation Management
;; Rebind to counsel functions
(use-package counsel
  :bind (("M-x" . 'counsel-M-x)
         ("C-x b" . 'counsel-switch-buffer)
         ("C-x C-f" . 'counsel-find-file))
  :config
  (counsel-mode 1)
  )

;; --------------------------------------------------------------------------------
;; * Ivy - Completion framework ----------------------------
(use-package ivy
  ;; minor mode name will not display in mode line but will still be active
  :diminish
  :bind (("C-f" . swiper) ;; Common find binding
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  ;; Set up keybindings
  (ivy-mode 1)
	;; Remove ^ which Ivy uses as a default input in counsel-M-x
	;; so search will be by substring rather than requiring the command
	;; to begin with the first search string
	;; https://emacs.stackexchange.com/questions/38841/counsel-m-x-always-shows
	(setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
	;; For Ivy completions, ignore order for regex to allow matches
	;; to be in any order. See options and defaults at https://oremacs.com/swiper/
	(setq ivy-re-builders-alist
				'((t . ivy--regex-ignore-order)))
	)

;; Completion descriptions for commands
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; Show most used commands using M-x, used by counsel M-x
(use-package amx
  :after counsel)

;; --------------------------------------------------------------------------------
;; * Key Bindings ----------------------------

;; VS Code Like Acccessible Commands
;; Call commands with F1
(global-set-key [f1] 'counsel-M-x)

;; Browse recently opened files - overwrite minimal settings
(global-set-key (kbd "C-r") 'counsel-recentf)

;; Bind switch buffer to C-tab - overwrite minimal settings
(global-set-key (kbd "<C-tab>") 'counsel-switch-buffer)


(provide 'jt-emacs-completion-ivy-counsel)

;;; jt-emacs-completion-ivy-counsel.el ends here
