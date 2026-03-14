;; from modules/jt/music/config.el
(use-package! bongo
  :defer t
  :commands bongo
  :config
  ;; Set mpv was preferred backend instead of (vlc mpv)
  ;; (setq bongo-enabled-backends '(mpv))
  )
