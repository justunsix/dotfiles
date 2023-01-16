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


;; --------------------------------------------------------------------------------
;; * Org Present --------------------------

(with-eval-after-load 'org
  ;; Package to centre text
  (use-package visual-fill-column)
  (use-package org-present)

  ;; Visual-fill-column variables are buffer local variables
  ;; and must set with defaults to apply globally
  (setq-default visual-fill-column-width 220)
  (setq-default visual-fill-column-center-text t)

  ;; Center text on screen in org-present
  (defun efs/org-present-start ()

    ;; Turn off centaur-tabs locally in buffer
    (centaur-tabs-local-mode t)

    ;; Centre presentation and wrap lines
    (visual-fill-column-mode 1)
    (visual-line-mode 1)
    ;; Larger font sizes for presentations
    ;; Relative to existing font heights
    (setq-local face-remapping-alist '((default (:height 1.6) variable-pitch)
                                       (header-line (:height 2.0) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))

    ;; Ensure images are displayed in presentation
    (org-toggle-inline-images)

    ;; Set a blank header line string to create blank space at the top
    (setq header-line-format " ")

    ;; Hide unneeded UI elements (this can even be done in my/org-present-start!)
    (menu-bar-mode 0)
    (tool-bar-mode -1)
    (scroll-bar-mode 0)
    ;; Let the desktop background show through
    ;; (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
    ;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

    ;; Adjust font for presentation friendly fonts
    ;; (set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 120)
    ;; (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 120)
    ;; (set-face-attribute 'variable-pitch nil :font "JetBrains Mono" :weight 'light :height 1.3)

    (set-face-attribute 'default nil :font "Iosevka" :weight 'light :height 120)
    (set-face-attribute 'fixed-pitch nil :font "Iosevka" :weight 'light :height 120)
    (set-face-attribute 'variable-pitch nil :font "Iosevka" :weight 'light :height 1.3)


    ;; Dark Theme for presentations
    ;; (load-theme 'doom-palenight t)

    ;; Light Theme for presentations
    (load-theme 'doom-tomorrow-day t)

    ;; Disable mode line just for presentation buffer
    (setq mode-line-format nil)

    )

  (defun efs/org-present-stop ()

    ;; Re-enable centaur-tabs locally in buffer
    (centaur-tabs-local-mode -1)

    ;; Stop document centre justification
    (visual-fill-column-mode 0)
    ;; Reset font changes
    (setq-local face-remapping-alist '((default variable-pitch default)))
    ;; Clear the header line format by setting to `nil'
    (setq header-line-format nil)
    ;; Turn  UI elements back on
    (menu-bar-mode t)
    (tool-bar-mode 0)
    (scroll-bar-mode t)

    ;; Reset fonts to defaults
    (jt/set-default-fonts)
    ;; Go back to regular theme
    (load-theme 'doom-tokyo-night t)
    )

  ;; Prepare slides with multiple headings to collapse them by default
  ;; Open them during the presentation
  (defun efs/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)

    ;; Unfold the current entry
    (org-show-entry)

    ;; Show only direct subheadings of the slide but don't expand them
    (org-show-children)
    )

  ;; Register hooks with org-present so start and stop configuration
  ;; functions run
  (add-hook 'org-present-mode-hook 'efs/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'efs/org-present-stop)

  ;; Prepare slides by collapsing headings
  ;;(add-hook 'org-present-after-navigate-functions 'efs/org-present-prepare-slide)
  ;; Remove hook for now
  (setq org-present-after-navigate-functions nil)

  )

;; --------------------------------------------------------------------------------
;; * Spelling - Writing: Languages Spelling ----------------------------
;; from https://github.com/munen/emacs.d/#flyspell

;; Order corrections by likeliness
;; Do not order not by the default of alphabetical ordering.
(setq flyspell-sort-corrections nil)  

;; Do not print messages for every word
;; When checking the entire buffer, don’t print messages for every word. This is a major performance gain.
(setq flyspell-issue-message-flag nil)

;; if Windows, set ispell-hunspell-dict-paths-alist to "C:/Hunspell"
(when jt/windows-p
  (setq ispell-hunspell-dict-paths-alist '(("en_US" "C:/Hunspell/en_US.aff" "C:/Hunspell/en_US.dic")
                                           ("en_GB" "C:/Hunspell/en_GB.aff" "C:/Hunspell/en_GB.dic")
                                           ("fr_FR" "C:/Hunspell/fr.aff" "C:/Hunspell/fr.dic")
                                           ("fr_CA" "C:/Hunspell/fr.aff" "C:/Hunspell/fr.dic")																								
                                           ))
  )

;; hunspell with multiple dictionaries
;; Ensure dictories are installed with OS package managers
(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  ;; Configure US, British variants of English, International and Canadian French
  (setq ispell-dictionary "en_GB,en_US,fr_FR,fr_CA")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB,en_US,fr_FR,fr_CA")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  ;; The personal dictionary file has to exist, otherwise hunspell will silently not use it.
  ;; Make sure file is present

  (ispell-change-dictionary "en_GB,en_US,fr_FR,fr_CA")
  (setq ispell-personal-dictionary "~/.config/hunspell/.hunspell_personal")    
  )
;; Run flyspell-mode in org-mode and markdown-mode
;; where spelling assistance is needed
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; 
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

(provide 'extras)
