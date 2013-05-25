(require 'mode-line-frame)

(defface nav/elscreen-mlf-active-face
  '((t (:foreground "#FF6400")))
  "Face used for active screen in mode line frame")

(defface nav/elscreen-mlf-inactive-face
  '((t (:foreground "#4c83ff")))
  "Face used for inactive screen in mode line frame")

(defface nav/elscreen-mlf-extra-face
  '((t (:foreground "gray30")))
  "Face used for extra chars in mode line frame")

(defun nav/elscreen-mlf-construct-frame-line ()
  (mapconcat
   (lambda (num)
     (propertize (or (elscreen-get-screen-nickname num) (number-to-string num))
                 'face
                 (if (= num (elscreen-get-current-screen))
                     'nav/elscreen-mlf-active-face
                   'nav/elscreen-mlf-inactive-face)))
   (reverse (elscreen-get-screen-list))
   (propertize " | " 'face 'nav/elscreen-mlf-extra-face)))

(setq mode-line-frame-format
      (list " "
            '(:eval (nav/elscreen-mlf-construct-frame-line))
            '(:propertize " | " face nav/elscreen-mlf-extra-face)
            '(:propertize nav/emms-lyrics-current-line face nav/elscreen-mlf-active-face)))

(setq mode-line-frame-parameters
      '((title . "mode-line-frame")
	(name . "mode-line-frame")
	(cursor-type . nil)
	(minibuffer . nil)
	(mode-line . nil)
	(height . 1)
	(width . 168)
	(user-size . t)
	(top . 0)
	(user-position . t)
	(border-width . 0)))

(setq initial-frame-alist (append initial-frame-alist '((height . 41)
							(width . 169)
							(user-size . t)
							(top . 22)
							(user-position . t))))

(add-hook 'elscreen-screen-update-hook 'mlf-updater)

(add-hook 'after-init-hook (lambda ()
                             (interactive)
                             (mode-line-frame-create)
                             (set-frame-parameter
                              mlf-frame
                              'background-color "gray10")))
