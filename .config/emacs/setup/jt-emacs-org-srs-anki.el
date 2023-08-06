;;; jt-emacs-org-srs-anki.el --- Personal spaced repetition system (SRS) and Anki org-mode packages
;;  Author: Justin Tung
;;; Commentary:
;; Configurations related packages in org-mode that do spaced repetition system (SRS) and Anki
;;; Code:

;; Suggestions in this file from Cheong Yiufung in post:
;; Power up Anki with Emacs, Org mode, anki-editor and more
;; https://yiufung.net/post/anki-org/

;; --------------------------------------------------------------------------------
;; * Anki Editor Setup --------------------------
;; anki-editor connects Emacs org-mode to AnkiConnect

(use-package anki-editor
	:after org
	:config
	;; Allow anki-editor to create a new deck if it doesn't exist
	(setq anki-editor-create-decks t
				anki-editor-org-tags-as-anki-tags t)
	;; Org-capture templates
	;; Example from: https://github.com/yiufung/dot-emacs/blob/master/init.el#L1859
	(setq org-my-anki-file (expand-file-name "Learning-Anki-Master-Deck.org" org-directory))
	)

(provide 'jt-emacs-org-srs-anki)

;;; jt-emacs-org-srs-anki.el ends here
