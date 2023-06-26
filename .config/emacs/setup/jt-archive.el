;; Archive of Emacs configuration
;; Replaced by other configuration or packages

;; Date, date time replaced by yassnippet date/time snippets
(defun jt/insert-date ()
  "Insert the current date in format YYYY-MM-DD into the current buffer"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun jt/insert-date-time ()
  "Insert the current date in format YYYY-MM-DD HHMM into the current buffer"
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H%M")))

(global-set-key (kbd "<f2> i d") 'jt/insert-date)

(global-set-key (kbd "<f2> i t") 'jt/insert-date-time)
