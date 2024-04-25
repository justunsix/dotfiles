;; jt-emacs-abbrev.el --- Personal shorthand abbreviations for Emacs
;;  Author: Justin Tung
;;; Commentary:
;; Abbreviations to expand using Emacs abbrev-mode
;;; Code:

;; Use modified Teeline shorthand for typed text
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
    ;; -- Common words
    ("za" "and" nil :count 0)
    ("zr" "are" nil :count 0)
    ("zu" "you" nil :count 0)
    ("wd" "would" nil :count 0)
    ("wc" "which" nil :count 0)
    ("zl" "will" nil :count 0)
    ("zw" "with" nil :count 0)
    ("ur" "your" nil :count 0)
    ("zs" "this" nil :count 0)
    ("zt" "the" nil :count 0)
    ("tt" "that" nil :count 0)
    ("z4" "for" nil :count 0)
    ;; - from
    ;; - has/have
    ;; - the
    ;; - our

    ;; English words or names using
    ;; Teeline rules
    ("avlble" "available" nil :count 0)
    ("bbstr" "babysitter" nil :count 0)
    ("cld" "cloud" nil :count 0)
    ("dwnld" "download" nil :count 0)
    ("eltr" "electro" nil :count 0)
    ("hppy" "happy" nil :count 0)
    ("intrnt" "internet" nil :count 0)
    ("intrvw" "interview" nil :count 0)
    ("mgmt" "management" nil :count 0)
    ("mgntc" "magnetic" nil :count 0)
    ("astrln" "austrialian" nil :count 0)
    ("bckgrnd" "background" nil :count 0)
    ("brtsh" "british" nil :count 0)
    ("chrldr" "cheerleader" nil :count 0)
    ("chrstms" "christmas" nil :count 0)
    ("dtbse" "database" nil :count 0)
    ("gls" "glass" nil :count 0)
    ("glss" "glasses" nil :count 0)
    ("gme" "game" nil :count 0)
    ("gymnstcs" "gymnastics" nil :count 0)
    ("hlwn" "halloween" nil :count 0)
    ;; ("hr" "hair" nil :count 0)
    ("htl" "hotel" nil :count 0)
    ("lbrry" "library" nil :count 0)
    ("ntwrk" "network" nil :count 0)
    ("nrse" "nurse" nil :count 0)
    ("ofce" "office" nil :count 0)
    ("prent" "parent" nil :count 0) ;; different rule vs print
    ("prnt" "print" nil :count 0)
    ("tchr" "teacher" nil :count 0)
    ("ttoo" "tattoo" nil :count 0)
    ("srvr" "server" nil :count 0)
    ("srvrs" "servers" nil :count 0)
    ("wtchng" "watching" nil :count 0)

    ;; -- Domain specific words or abbreviations
    ("ZMS" "Microsoft" nil :count 0)
    ("AZ" "Azure" nil :count 0)

    ;; -- Custom Abbreviations
    ("GF" "girlfriend" nil :count 0)
    ("OPP" "opportunity" nil :count 0)
    ("REQS" "requirements" nil :count 0)
    ("ZAI" "artificial intelligence" nil :count 0)

    ;; --- Countries
    ("ZAUS" "australia" nil :count 0)
    ("ZCAD" "canada" nil :count 0)
    ("ZJAP" "japan" nil :count 0)
    ("ZPHIL" "philippines" nil :count 0)
    ("ZRUS" "russia" nil :count 0)

    ))

(provide 'jt-emacs-abbrev)

;;; jt-emacs-abbrev.el ends here
