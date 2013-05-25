(require 'emms-browser)

(setq emms-browser-alpha-sort-function 'emms-browser-sort-by-year-or-name)

(defun nav/emms-browser-remove-current-node ()
  "Remove the current node, and empty parents.
Also remove all tracks under node from cache."
  (interactive)
  (let ((tracks (emms-browser-tracks-at-point)) path)
    (dolist (track tracks)
      (setq path (emms-track-get track 'name))
      (nav/emms-names-cache-del path)
      (emms-cache-del path)))
  (emms-browser-delete-current-node))

(defun nav/emms-add-dired-and-unmark ()
  "Add all marked directories and unmark all marks"
  (interactive)
  (emms-add-dired)
  (dired-unmark-all-marks))

;; My preference for how to display albums/tracks in browser
(setq emms-browser-info-album-format "%i%n")
(setq emms-browser-info-title-format "%i%t")

;;; Patches
(defun emms-browser-add-tracks ()
  "Add all tracks at point.
Return the previous point-max before adding."
  (interactive)
  (let ((first-new-track (with-current-emms-playlist (point-max)))
        (bdata (emms-browser-bdata-at-point)))
    (emms-browser-playlist-insert-bdata
     bdata (emms-browser-bdata-level bdata))
    (run-hook-with-args 'emms-browser-tracks-added-hook
                        first-new-track)
    (with-current-emms-playlist
      (if (= 1 (point))
	  (emms-playlist-select (point))))
    first-new-track))
