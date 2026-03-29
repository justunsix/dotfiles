;; from modules/jt/minimal/autoload.el
;;;###autoload
(defun jt/create-org-note (title)
  "Create new org file in current directory, add ID, and ask user for details.

Details are title, filetags. Filename will be the title with spaces replaced by hyphens."
  (interactive "sEnter the title of the org file: ")
  (let*
      ((filename (concat (replace-regexp-in-string " " "-" title) ".org"))
       (buffer (find-file filename))
       (tags (read-string "Enter tags separated by colon: "))
       (id-string (concat ":PROPERTIES:\n:ID: " (org-id-get-create) "\n:END:\n"))
       (title-string (concat "#+title: " title "\n"))
       (tags-string (concat "#+filetags: " tags "\n\n* See Also\n\n"))
       (file-link-string (concat "- [[file:" filename "][" title "]] - "))
       )
    (with-current-buffer buffer
      (insert id-string)
      (insert title-string)
      (insert tags-string)
      (insert file-link-string)
      )
    (save-buffer)
    )
  )

;;;###autoload
(defun jt/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the kill ring (clipboard)."
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename))
  )

;;;###autoload
(defun jt/find-dired-fuzzy (pattern)
  "Run find-dired with fuzzy -iname matching for PATTERN.

Takes the given PATTERN and inserts asterisks at the beginning and
end and around spaces to enable fuzzy matching of filenames in the current directory."
  (interactive "sEnter filename pattern: ")
  (let* (
         ;; Build fuzzy pattern: prepend/append asterisk, replace spaces with asterisk.
         ;; Example: "foo bar" -> "*foo*bar*"
         ;; The leading asterisk allows matching files that START with "foo".
         ;; The trailing asterisk allows matching files that END with the last word.
         ;; Replacing spaces with asterisks allows any characters between words.
         (fuzzy-pattern (concat "*" (replace-regexp-in-string " " "*" pattern) "*"))
         ;; Determine the directory to search:
         ;; If already in dired-mode, use its directory (the buffer's default-directory).
         ;; Otherwise, expand the current working directory to an absolute path.
         (default-directory (if (derived-mode-p 'dired-mode)
                                default-directory
                              (expand-file-name default-directory))))
    ;; let constrains find-dired to use -exec ls -ld {} + instead of -ls
    ;; to remove escaped spaces in find-dired buffer (\ )
    (let ((find-ls-option '("-exec ls -ld {} +" . "-ld")))
      (find-dired default-directory (concat "-iname \"" fuzzy-pattern "\"")))))
