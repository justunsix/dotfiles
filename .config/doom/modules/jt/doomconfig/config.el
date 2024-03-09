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
