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
