;; from modules/jt/music/autoload.el
;;;###autoload
;; Location of your bongo playlist file (optional)
(defun jt/bongo-open-my-playlist()
  ;; 	"Open my playlist in bongo stored in playlist environment variable"
 	(interactive)
 	(bongo)
 	(bongo-insert-playlist-contents jt/bongo-playlist-location)
 	(bongo-playlist-mode)
 	)