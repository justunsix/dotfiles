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

;;;###autoload
(defun jt/sort-lines-buffer ()
  "Sort all lines in the current buffer and save changes."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (sort-lines nil (point-min) (point-max)))
    (save-buffer))  
  )

(defvar jt-org-presentation-mode nil
  "Non-nil if presentation mode is enabled.")

;;;###autoload
(defun jt/org-toggle-presentation-mode ()
  "Toggle presentation mode settings."
  (interactive)
  (if jt-org-presentation-mode
      (progn
        ;; Revert to default settings
        (setq doom-font (font-spec :family "Jetbrains Mono" :size 15)):want
        (load-theme 'doom-tokyo-night t)
        (display-line-numbers-mode 1)
        (doom/reload-font)        
        (setq jt-org-presentation-mode nil)
        (message "Presentation mode disabled"))
    (progn
      ;; Apply presentation settings
      (setq doom-font (font-spec :family "Jetbrains Mono" :size 15))
      (load-theme 'doom-plain t)
      (display-line-numbers-mode -1)
      (doom/reload-font)
      (setq jt-org-presentation-mode t)
      (message "Presentation mode enabled"))))