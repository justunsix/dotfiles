;; jt-emacs-abbrev.el --- Personal shorthand abbreviations for Emacs
;;  Author: Justin Tung
;;; Commentary:
;; Abbreviations to expand using Emacs abbrev-mode
;;; Code:

;;-*-coding: utf-8;-*-
;; Started from http://xahlee.info/emacs/emacs/emacs_abbrev_mode_tutorial.html
(define-abbrev-table 'global-abbrev-table
  '(
    ;; English words
    ("zbg" "background" nil :count 0)
    ;; -- IT
    ("zdb" "database" nil :count 0)
   ))


(provide 'jt-emacs-abbrev)

;;; jt-emacs-abbrev.el ends here