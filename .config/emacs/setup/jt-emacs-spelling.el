;;; jt-emacs-spelling.el --- Personal spelling configurations and packages
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related to spelling
;;; Code:

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

(provide 'jt-emacs-spelling)

;;; jt-emacs-spelling.el ends here
