(require 'recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.

;; enable recent files mode.
(recentf-mode t)

                                        ; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defvar recentf-dir-list)
(put 'recentf-dir-list 'history-length 50)

(defun recentf-add-dir (dir)
  (let ((history-delete-duplicates t))
    (add-to-history 'recentf-dir-list dir)))

(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'recentf-dir-list)
