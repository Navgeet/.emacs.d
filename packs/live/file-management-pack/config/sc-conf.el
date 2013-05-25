(live-add-pack-lib "sunrise-commander")
(require 'sunrise-commander)

(defun nav/sr-goto-dir ()
  (interactive)
  (sr-goto-dir (ido-read-directory-name "cd: ")))

(define-key sr-mode-map (kbd "/") 'nav/sr-goto-dir)
