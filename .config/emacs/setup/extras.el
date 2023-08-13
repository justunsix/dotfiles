;; Extra Packages not needed for daily work

;; --------------------------------------------------------------------------------
;; * Focus Mode ----------------------------

;; Focus mode is a minor mode that hides all the clutter in your Emacs frame
;; leaving only the buffer you are editing.
;;
;; Based on: https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/ and
;; olivetti mode

(use-package olivetti
  :defer
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  (define-minor-mode prot/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters.
  ;; Fringes are disabled.  The modeline is hidden, except for
  ;; `prog-mode' buffers (see `prot/hidden-mode-line-mode').  The
  ;; default typeface is set to a proportionately-spaced family,
  ;; except for programming modes (see `prot/variable-pitch-mode').
  ;; The cursor becomes a blinking bar, per `prot/cursor-type-mode'."
    :init-value nil
    :global nil
    (if prot/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
          (prot/variable-pitch-mode 1)
          ;; (prot/cursor-type-mode 1)
          ;; (unless (derived-mode-p 'prog-mode)
          ;;  (prot/hidden-mode-line-mode 1))
          (text-scale-set 2)
          (setq-local face-remapping-alist '((default (:height 1.2) variable-pitch)
                                             (header-line (:height 1.4) variable-pitch))
                      )

          ;; Adjust font for variable pitch
          (set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 110)
          (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 110)
          (set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :weight 'light :height 1.3)

          )
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (prot/variable-pitch-mode -1)
      ;; (prot/cursor-type-mode -1)
      ;;(unless (derived-mode-p 'prog-mode)
      ;;     (prot/hidden-mode-line-mode -1))
      ;; Return text scale to normal
      (text-scale-set 0)

      (setq-local face-remapping-alist '((default variable-pitch default)))
      ;; Clear the header line format by setting to `nil'
      (setq header-line-format nil)

      (jt/set-default-fonts)

      )
    )
  :bind ("C-c o" . prot/olivetti-mode)
  )

(use-package face-remap
  ;; the actual mode
  :diminish buffer-face-mode
  :commands prot/variable-pitch-mode
  :config
  (define-minor-mode prot/variable-pitch-mode
    "Toggle `variable-pitch-mode', except for `prog-mode'."
    :init-value nil
    :global nil
    (if prot/variable-pitch-mode
        (unless (derived-mode-p 'prog-mode)
          (variable-pitch-mode 1)
          )
      (variable-pitch-mode -1))
    )
  )

;; --------------------------------------------------------------------------------
;;  ;; * Frame Management ----------------------------
;;  (use-package mini-frame
;;    )
;;
;;  ;; Those using vertical completion candidates list
;;  ;; may configure mini-frame not to occupy full width
;;  (custom-set-variables
;;   '(mini-frame-show-parameters
;;     '((top . 30)
;;       (width . 1.0)
;;       (left . 0.5))
;;     )
;;   )
;;
;;  ;; Gnome shell does not resize Emacs child frames
;;  ;; Until issue  solved, Gnome Shell users must also set mini-frame height.
;;  ;; Another option for Gnome Shell users is to use the following code in initialization file:
;;  (setq x-gtk-resize-child-frames 'resize-mode)
;;
;;  ;; Configure the list of commands that must not be shown in the child frame
;;  ;; by customizing the mini-frame-ignore-commands (e.g. swiper to see source contents)
;;  (setq mini-frame-ignore-commands
;;        '(swiper counsel-switch-buffer))
;;
;;  (mini-frame-mode)

;; * Nyan mode (Cat file progress bar)
(use-package nyan-mode
	:ensure t
	:config
	(nyan-mode 1))

;; --------------------------------------------------------------------------------
;; Other switches
;; (nyan-start-animation)
;; (nyan-stop-animation)
;; (nyan-toggle-wavy-trail)

;; ranger
(use-package ranger
	:after dired
	:config
	;; Configurations per https://github.com/punassuming/ranger.el#configuration
	(setq
	 ;; kill the buffers, after you move to another entry in the dired buffer.
	 ranger-cleanup-eagerly t
	 ;; show hidden files, toggle with zh
	 ranger-show-hidden t
	 ;; size of the parent windows as a fraction of the frame size.
	 ranger-width-parents 0.12
	 ;; preview selected file / directory on startup
	 ranger-preview-file t
	 ;; Exclude Files From Being Previewed, including binaries
	 ranger-excluded-extensions '("mkv" "iso" "mp4")
	 ranger-dont-show-binary t
	 ;; Exclude Directories From Being Previewed by size in MB
   ranger-max-preview-size 4
	 )
	)
;; C-p activates (deer-from-dired), but conflicts with global key C-p

;; Neotree

(use-package neotree
	)

;; Use icons for window system and arrow terminal
;; per https://github.com/jaypei/emacs-neotree
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(provide 'extras)
