;;; Play songs using ido

(defvar nav/emms-names-cache-db (make-hash-table
			     :test 'equal)
  "A mapping of titles to paths.
Used to offer completions for songs in minibuffer.")

(defvar nav/emms-path-to-names-db (make-hash-table
			     :test 'equal)
  "A mapping of paths to titles.")

(defvar nav/emms-track-history-names nil
  "History of songs played, stored as their names.")

;; no limit on history list length
(put 'nav/emms-track-history-names 'history-length t)

;;; Functions
(defun nav/emms-names-cache-del (path)
  (let ((name (gethash path nav/emms-path-to-names-db)))
    (remhash name nav/emms-names-cache-db)
    (remhash path nav/emms-path-to-names-db)
    (setq nav/emms-track-history-names (delete name nav/emms-track-history-names))))

(defun nav/emms-create-new-title (title path)
  (interactive)
  (let* ((list (gethash path emms-cache-db))
	 (artist (cdr (assoc 'info-artist list)))
	 (album (cdr (assoc 'info-album list))))
    (format "(%s)(%s) %s" artist album title)))

(defun nav/emms-path-to-names-db-rectify (new-title path)
  (puthash path new-title nav/emms-path-to-names-db))

(defun nav/emms-check-title (title path)
  "Check nav/emms-names-cache-db for same TITLE (case insensitive). If found,
prefix (artist)(album) to TITLE and insert. Do this for every match."
  (let ((pos 0)
	(list nav/emms-track-history-names))
    (while (and list (not (string= (downcase (car list)) (downcase title))))
      (setq list (cdr list))
      (setq pos (1+ pos)))
    (if list
	(let* ((other-path (gethash (car list) nav/emms-names-cache-db))
	       (new-title (nav/emms-create-new-title
			   (car list)
			   other-path)))
	  (setcar (nthcdr pos nav/emms-track-history-names) new-title)
	  (remhash (car list) nav/emms-names-cache-db)
	  (puthash new-title other-path nav/emms-names-cache-db)
	  (nav/emms-path-to-names-db-rectify new-title other-path)
	  (nav/emms-create-new-title title path))
      title)))

(defun nav/emms-add-track-to-names-cache-db (track)
  "Update the names db when a track is added to browser."
  (let* ((title (cdr (assoc 'info-title track)))
	 (path (cdr (assoc 'name track)))
	 (full-title (nav/emms-check-title title path)))
    ;; track to be added has been added already, update it
    (when (and nav/emms-path-to-names-db (gethash path nav/emms-path-to-names-db))
      (nav/emms-names-cache-del path)
      (setq full-title (nav/emms-check-title title path)))

    (add-to-list
     'nav/emms-track-history-names
     full-title t)
    (puthash full-title path nav/emms-names-cache-db)
    (puthash path full-title nav/emms-path-to-names-db)))

(defun nav/emms-make-names-cache-db ()
  "Utility function to create names-cache-db from emms-cache-db"
  (interactive)
  (maphash (lambda (key value)
	     (let* ((title (cdr (assoc 'info-title value)))
		    (full-title (nav/emms-check-title title key)))
	       (puthash full-title key nav/emms-names-cache-db)
	       (puthash key full-title nav/emms-path-to-names-db)))
	   emms-cache-db))

(defun nav/emms-add-track-name-to-history-list (track)
  (let ((history-delete-duplicates t)
        (name (gethash
               (emms-track-get track 'name)
               nav/emms-path-to-names-db)))
    (add-to-history 'nav/emms-track-history-names name)))

(defun emms-browser-add-tracks-non-interactive (bdata)
  "Add all tracks at point.
Return the previous point-max before adding."
  (let ((first-new-track (with-current-emms-playlist (point-max))))
    (emms-browser-playlist-insert-bdata
     bdata (emms-browser-bdata-level bdata))
    (run-hook-with-args 'emms-browser-tracks-added-hook
                        first-new-track)
    (with-current-emms-playlist
      (if (= 1 (point))
	  (emms-playlist-select (point))))
    first-new-track))

;; Main entry point
(defun nav/emms-play-interactively ()
  "Select a track from the minibuffer using `completing-read-ido'."
  (interactive)
  (let* ((history-add-new-input nil)
	 (song (ido-completing-read "Play: " nav/emms-track-history-names))
	 (path (gethash song nav/emms-names-cache-db))
	 (track (gethash path emms-cache-db))
	 (name (emms-browser-make-name `(,path ,track) 'info-title))
	 (bdata `((type . info-title)
		  (level . 3)
		  (name . ,name)
		  (data . (,track))))
	 (emms-browser-current-indent "")
	 (old-pos (emms-browser-add-tracks-non-interactive bdata)))
  (if (eq last-command-event 12)
      (with-current-emms-playlist
	(goto-char old-pos)
	;; if we're sitting on a group name, move forward
	(unless (emms-playlist-track-at (point))
	  (emms-playlist-next))
	(emms-playlist-select (point))
	(emms-stop)
	(emms-start)))
  (unless emms-player-playing-p (emms-start))))

(defun nav/populate-track-history-names ()
  "Populate the nav/emms-track-history-names list."
  (interactive)
  (setq nav/emms-track-history-names nil)
  (maphash (lambda (key value)
	     (setq nav/emms-track-history-names
                   (cons key nav/emms-track-history-names)))
	   nav/emms-names-cache-db))

(add-hook 'emms-track-initialize-functions 'nav/emms-add-track-to-names-cache-db t)
(add-hook 'nav/emms-player-started-hook
          (lambda (track)
            (setq nav/emms-currently-playing-track track)
            (later-do 'nav/emms-add-track-name-to-history-list track)))

(provide 'emms-iselect)
;;; emms-iselect.el ends here
