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
  (setq projectile-project-search-path '("~/Code"))

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

  ;; Set file types that will open in Emacs from org file links
  (add-to-list 'org-file-apps '("\\.puml\\'" . emacs))

  )

(after! plantuml-mode
  ;; Enable plantuml-mode for PlantUML files .puml
  ;; auto-mode-alist already has other PlantUML formats
  ;; https://github.com/skuro/plantuml-mode
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  )

(after! org-roam
  ;; Set org-roam-directory, could be before org-roam loads
  ;; https://github.com/doomemacs/doomemacs/issues/4130
  (setq org-roam-directory (file-truename jt/org-directory))
  ;; Disable completions
  (setq org-roam-completion-everywhere nil)
  )

;; Spelling
(after! ispell
  (setenv "LANG" "en_CA.UTF-8")
  (when (eq system-type 'windows-nt)
    (setq ispell-alternate-dictionary "~/.config/mydict/words.txt")
    ))

;; Spelling - Completion
(after! cape
  ;; If on Windows where /usr/share/dict/words is not available.
  (when (eq system-type 'windows-nt)
    (setq cape-dict-file "~/.config/mydict/words.txt")
    ))

;; Make evil snipe search visible buffer
(after! evil-snipe
  (setq! evil-snipe-scope 'whole-visible)
  )

;; Enable jk as escape from Evil insert mode
;; Solution from: https://github.com/doomemacs/doomemacs/issues/8337
(after! evil-escape
  (setq evil-escape-key-sequence "jk"))

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

(after! evil
  ;; Per https://github.com/doomemacs/doomemacs/issues/1643#issuecomment-519288725
  ;; and callable function map!
  ;; Replace g w > evil-fill with avy go to word in normal mode
  (map!
   :n "g w" #'avy-goto-word-1
   ))
