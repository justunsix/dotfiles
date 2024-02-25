;; jt-emacs-abbrev.el --- Personal shorthand abbreviations for Emacs
;;  Author: Justin Tung
;;; Commentary:
;; Abbreviations to expand using Emacs abbrev-mode
;;; Code:

;; Follow modified Teeline shorthand for typed text
;; https://www.wikihow.com/Write-Shorthand#Teeline-Shorthand
;; Stage 1.
;; - Leave out
;;   - Middle vowels (AEIOU)
;;   - Double vowels
;;   - Double consonants
;; - Keep vowels at the beginning and end of a word
;; Stage 2.
;; - Exclude silent letters
;; - Use brief forms for common words
;; - Use abbreviations

;; Tips from:
;; - http://xahlee.info/emacs/emacs/emacs_abbrev_mode_tutorial.html
;; - https://www.emacswiki.org/emacs/AbbrevMode

;;-*-coding: utf-8;-*-
(define-abbrev-table 'global-abbrev-table
  '(
    ;; English words
    ("astrln" "austrialian" nil :count 0)
    ("bckgrnd" "background" nil :count 0)
    ("brtsh" "british" nil :count 0)
    ("chrldr" "cheerleader" nil :count 0)
    ("dtbse" "database" nil :count 0)
		("eltr" "electro" nil :count 0)
    ("gymnstcs" "gymnastics" nil :count 0)
		("hppy" "happy" nil :count 0)
    ("jpnse" "japanese" nil :count 0)
		("mgntc" "magnetic" nil :count 0)
    ("nrse" "nurse" nil :count 0)
    ("tchr" "teacher" nil :count 0)

    ;; -- Abbreviations
    ("OPP" "opportunity" nil :count 0)
    ("GF" "girlfriend" nil :count 0)

    ;; -- Common words
    ("=" "and" nil :count 0)
    ("r" "are" nil :count 0)
		("u" "you" nil :count 0)
		("wd" "would" nil :count 0)
		("wc" "which" nil :count 0)
		("l" "will" nil :count 0)
    ("w" "with" nil :count 0)
    ("ur" "your" nil :count 0)
    ("s" "this" nil :count 0)
    ("t" "the" nil :count 0)
    ("tt" "that" nil :count 0)
    ("4" "for" nil :count 0)
		;; - from
		;; - has/have
		;; - the
		;; - our
		))

(provide 'jt-emacs-abbrev)

;;; jt-emacs-abbrev.el ends here
