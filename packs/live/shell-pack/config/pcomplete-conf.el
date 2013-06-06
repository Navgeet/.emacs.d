(defun nav/pcomplete ()
  (interactive)
  (catch 'get-back
    (pcomplete 1)))

(add-hook 'eshell-mode-hook (lambda ()
                              (interactive)
                              (define-key eshell-mode-map [tab] 'nav/pcomplete)))

(defun pcomplete/cd ()
  (move-end-of-line 1)
  (save-match-data
    (let (dir init dirr str len)
      (if (save-excursion (looking-back "[^\\\\] \\([^/]*\\)"))
          (progn
            (setq dirr nil)
            (setq init (match-string 1))
            (setq len (length init)))

        (save-excursion (re-search-backward "[^\\\\] \\(.*\\)/\\(.*\\)"))
        (setq dir (match-string 1))
        (setq init (match-string 2))
        (setq dirr (if (equal "" dir) "/" dir))
        (setq len ((+ (length dir) (length init) 1))))

      (setq str (ido-read-directory-name "Dir: " dirr
                                         nil nil
                                         init))
      (delete-backward-char len)
      (insert (replace-regexp-in-string " " "\\\\ " str))
      (move-end-of-line 1)
      (throw 'get-back nil))))

;; Patches
(defun pcomplete-stub (stub candidates &optional cycle-p)
  "Dynamically complete STUB from CANDIDATES list.
This function inserts completion characters at point by completing
STUB from the strings in CANDIDATES.  A completions listing may be
shown in a help buffer if completion is ambiguous.

Returns nil if no completion was inserted.
Returns `sole' if completed with the only completion match.
Returns `shortest' if completed with the shortest of the matches.
Returns `partial' if completed as far as possible with the matches.
Returns `listed' if a completion listing was shown.

See also `pcomplete-filename'."
  (let* ((completion-ignore-case pcomplete-ignore-case)
	 (completions (all-completions stub candidates))
         (entry (try-completion stub candidates))
         result)
    (cond
     ((null entry)
      (if (and stub (> (length stub) 0))
          (message "No completions of %s" stub)
        (message "No completions")))
     ((eq entry t)
      (setq entry stub)
      (message "Sole completion")
      (setq result 'sole))
     ((= 1 (length completions))
      (setq result 'sole))
     ((and pcomplete-cycle-completions
           (or cycle-p
               (not pcomplete-cycle-cutoff-length)
               (<= (length completions)
                   pcomplete-cycle-cutoff-length)))
      (let ((bound (car (completion-boundaries stub candidates nil ""))))
        (unless (zerop bound)
          (setq completions (mapcar (lambda (c) (concat (substring stub 0 bound) c))
                                    completions)))
        (setq entry (car completions)
              pcomplete-current-completions completions)))
     ((and pcomplete-recexact
           (string-equal stub entry)
           (member entry completions))
      ;; It's not unique, but user wants shortest match.
      (message "Completed shortest")
      (setq result 'shortest))
     ((or pcomplete-autolist
          (string-equal stub entry))
      ;; It's not unique, list possible completions.
      ;; FIXME: pay attention to boundaries.
      ;; (pcomplete-show-completions completions)
      (let* ((init-str (when (equal (car completions) stub)
                         stub))
             (match (ido-completing-read "Complete: " completions
                                         nil nil
                                         init-str)))
        (when init-str
          (delete-backward-char (length init-str)))
        (insert match))
      (setq result 'listed))
     (t
      (message "Partially completed")
      (setq result 'partial)))
    (cons result entry)))
