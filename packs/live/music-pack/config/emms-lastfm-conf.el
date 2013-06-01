(require 'emms-lastfm-scrobbler)

(emms-lastfm-scrobbler-enable)

;; Patches
(defun emms-lastfm-scrobbler-stop-hook ()
  "Submit the track to last.fm if it has been played for 240
seconds or half the length of the track."
  (let ((current-track (emms-playlist-current-selected-track)))
    (let ((track-length (emms-track-get current-track 'info-playing-time)))
      (when (and track-length
		 (emms-lastfm-scrobbler-allowed-track-type current-track))
	(when (and
	       ;; track must be longer than 30 secs
	       (> track-length 30)
	       ;; track must be played for more than 240 secs or
	       ;;   half the tracks length, whichever comes first.
	       (> nav/emms-mode-line-time-elapsed-num
                  (min 240 (/ track-length 2))))
	  (emms-lastfm-scrobbler-make-async-submission-call
	   current-track nil))))))
