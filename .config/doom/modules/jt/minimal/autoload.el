;; from modules/jt/minimal/autoload.el
;;;###autoload
(defun jt/create-org-note (title)
  "Create a new org file in current directory, add an ID, prompt user for
	title, filetags. Filename will be the title with spaces replaced by hyphens."
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

;; Get name of current file without full path and copy to kill ring
;;;###autoload
(defun jt/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the kill ring (clipboard)."
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename))
  )
