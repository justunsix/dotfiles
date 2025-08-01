;;; $DOOMDIR/modules/jt/doomconfig/config.el -*- lexical-binding: t; -*-
;; Additional configuration for packages in doom-emacs framework

;; --------------------------------------------------------------------------------
;; * Package Configurations  ----------------------------

(after! which-key
  (setq which-key-idle-delay 0.2)
  )

(after! yasnippet-snippets
  ;; Add (yasnippet backquote-change) to ‘warning-suppress-types’
  ;; to turn off warnings of elisp executions in snippets
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  )

(after! projectile
  ;; Set this to the folder where you keep your repositories and projects
  (setq projectile-project-search-path jt/project-search-path)
  ;; Manually trigger projectile-discover-projects-in-search-path
  ;; to update the project list
  )

(after! org
  ;; https://orgmode.org/manual/Conflicts.html
  ;; shift-selection can select regions
  (setq org-support-shift-select t)
  ;; Prompt before babel code block execution
  (setq
   org-confirm-babel-evaluate t)

  )

(after! org-roam
  ;; Set org-roam-directory, could be before org-roam loads
  ;; https://github.com/doomemacs/doomemacs/issues/4130
  (setq org-roam-directory (file-truename jt/org-directory))
  ;; Disable completions
  (setq org-roam-completion-everywhere nil)
  )

;; Spelling
(after! flyspell
  (setenv "LANG" "en_CA.UTF-8")
  )

;; Make evil snipe search visible buffer
(after! evil-snipe
  (setq! evil-snipe-scope 'whole-visible)
  )

(after! magit
  ;; For use with magit-list-repositories
  ;; - List of directories that are or contain Git repositories.
  ;; - Format: directory . depth of directory where 0 is just directory itself
  (setq magit-repository-directories
        '(("~/Code/" . 1)
          ("~/Code/External" . 1)))
  ;; - Customize format of magit-list-repositories
  (setq magit-repolist-columns
        '(("Name"    25 magit-repolist-column-ident ())
          ("Version" 25 magit-repolist-column-version ())
          ("Flag" 3 magit-repolist-column-flag ())
          ;; ("D"        1 magit-repolist-column-dirty ())
          ("B<U"      3 magit-repolist-column-unpulled-from-upstream
           ((:right-align t)
            (:help-echo "Upstream changes not in branch")))
          ("B>U"      3 magit-repolist-column-unpushed-to-upstream
           ((:right-align t)
            (:help-echo "Local changes not in upstream")))
          ("Path"    99 magit-repolist-column-path ())))
  )

(after! nix-mode
  ;; Per https://github.com/oxalica/nil
  (setq lsp-nix-nil-formatter ["nixfmt"])
  )

;; ** Key maps

;; Per https://github.com/doomemacs/doomemacs/issues/1643#issuecomment-519288725
;; and callable function map!
;; Replace g w > evil-fill with avy go to word in normal mode
(map!
 :n "g w" #'avy-goto-word-1
 )
