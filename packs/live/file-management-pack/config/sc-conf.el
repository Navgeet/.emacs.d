(live-add-pack-lib "sunrise-commander")
(require 'sunrise-commander)

(defun nav/sr-goto-dir ()
  (interactive)
  (sr-goto-dir (ido-read-directory-name "cd: ")))

(defun nav/sr-goto-recent-dir ()
  (interactive)
  (let ((dir (ido-completing-read "cd: " recentf-dir-list nil t)))
    (when dir
      (sr-goto-dir dir))))

(defadvice sr-goto-dir (around sr-goto-dir-around activate)
  ad-do-it
  (recentf-add-dir dir))
