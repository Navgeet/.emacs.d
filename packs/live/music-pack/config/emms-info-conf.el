(require 'emms-info-metaflac)
(require 'emms-info-mp3info)

(add-to-list 'emms-info-functions 'emms-info-metaflac)
(add-to-list 'emms-info-functions 'emms-info-mp3info)

(setq emms-info-asynchronously nil)

;;; Patches
(setq emms-info-metaflac-options
  '("--no-utf8-convert"
    "--show-tag=TITLE"
    "--show-tag=ARTIST"
    "--show-tag=ALBUM"
    "--show-tag=DATE"
    "--show-tag=YEAR"
    "--show-tag=TRACKNUMBER"
    "--show-tag=GENRE"))

(defun emms-info-metaflac (track)
  "Get the FLAC tag of file TRACK, using `emms-info-metaflac-program'
and return an emms-info structure representing it."
  (when (and (eq 'file (emms-track-type track))
             (string-match "\\.\\(flac\\|FLAC\\)\\'" (emms-track-name track)))
    (with-temp-buffer
      (when (zerop
             (apply 'call-process
              emms-info-metaflac-program-name
              nil t nil
              "--show-total-samples"
              "--show-sample-rate"
              (append emms-info-metaflac-options
                      (list (emms-track-name track)))))
        (goto-char (point-min))
        (emms-track-set track 'info-playing-time
                        (/ (string-to-number (buffer-substring (point) (line-end-position)))
                           (progn
                             (forward-line 1)
                             (string-to-number (buffer-substring (point) (line-end-position))))))
        (forward-line 1)
	(while (looking-at "^\\([^=\n]+\\)=\\(.*\\)$")
          (let* ((str (cond
		      ((string= (match-string 1) "DATE") "year")
		      (t (downcase (match-string 1)))))
		(name (intern (concat "info-" str)))
                (value (match-string 2)))
            (when (> (length value)
                     0)
              (emms-track-set track
                              name
                              (if (eq name 'info-playing-time)
                                  (string-to-number value)
                                value))))
          (forward-line 1))))))
