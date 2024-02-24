;; jt-emacs-abbrev.el --- Personal shorthand abbreviations for Emacs
;;  Author: Justin Tung
;;; Commentary:
;; Abbreviations to expand using Emacs abbrev-mode
;;; Code:

;;-*-coding: utf-8;-*-
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
(define-abbrev-table 'global-abbrev-table
  '(
    ;; English words
    ("bckgrnd" "background" nil :count 0)
    ("dtbse" "database" nil :count 0)

    ;; -- Abbreviations
    ("OPP" "opportunity" nil :count 0)

    ;; -- Common words
    ("z+" "and" nil :count 0)
    ("zr" "are" nil :count 0)
    ("/w" "with" nil :count 0)
    ("ur" "your" nil :count 0)
   ))


(provide 'jt-emacs-abbrev)

;;; jt-emacs-abbrev.el ends here