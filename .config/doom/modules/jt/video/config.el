;; from modules/jt/video/config.el
(use-package! elfeed-tube-mpv
  :defer t
  :commands elfeed
  :config
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))
)
