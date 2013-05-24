(live-add-pack-lib "elscreen")
(require 'elscreen)

;;; Corrections
(defun elscreen-make-frame-confs (frame &optional keep-window-configuration)
  (when (null (elscreen-get-frame-confs frame))
    (let ((selected-frame (selected-frame))
          elscreen-window-configuration)
      (save-current-buffer
        (select-frame frame)
        (setq elscreen-window-configuration
              (if keep-window-configuration
                  (elscreen-current-window-configuration)
                (elscreen-default-window-configuration)))
        (elscreen--set-alist 'elscreen-frame-confs frame
                   (list
                    (cons 'screen-property
                          (list
                           (cons 1 (list
                                    (cons 'window-configuration
                                          elscreen-window-configuration)))))
                    (cons 'screen-history (list 1))
                    (cons 'modified-inquirer nil)
                    (cons 'screen-to-name-alist-cache nil)))
        (elscreen-apply-window-configuration elscreen-window-configuration)
        (elscreen-notify-screen-modification 'force-immediately)
        (select-frame selected-frame)))))

(defun elscreen-create-internal (&optional noerror)
  "Create a new screen.
If NOERROR is not nil, no message is displayed in mini buffer
when error is occurred."
  (cond
   ((>= (elscreen-get-number-of-screens) 9)
    (unless noerror
      (elscreen-message "No more screens."))
    nil)
   (t
    (let ((screen-list (sort (elscreen-get-screen-list) '<))
          (screen 1))
      (elscreen-set-window-configuration
       (elscreen-get-current-screen)
       (elscreen-current-window-configuration))
      (while (eq (nth (1- screen) screen-list) screen)
        (setq screen (+ screen 1)))
      (elscreen-set-window-configuration
       screen (elscreen-default-window-configuration))
      (elscreen-append-screen-to-history screen)
      (elscreen-notify-screen-modification 'force)
      (run-hooks 'elscreen-create-hook)
      screen))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'nav/elscreen-music-screen-layout)

(defvar nav/elscreen-music-screen-layout nil)

(defun nav/elscreen-music-screen-layout-save ()
  (interactive)
  (setq nav/elscreen-music-screen-layout (current-window-configuration)))

(defun nav/elscreen-init ()
  "Set up my usual window configuration."
  ;; 3 empty screens for work
  (elscreen-create)
  (elscreen-create)
  ;; 1 screen for music
  (elscreen-create)
  (elscreen-goto 4)
  ;; (nav/elscreen-music-screen-init)
  ;; finally..
  (elscreen-goto 1))

;; (defun nav/elscreen-music-screen-init ()
;;   (interactive)
;;   (elscreen-screen-nickname "music")
;;   ;; create buffers
;;   (emms-smart-browse)
;;   (emms-lyrics-create-buffer)
;;   (nav/emms-reco-similiar-tracks-create-buffer)
;;   (nav/emms-reco-top-tracks-create-buffer))
;;   ;; (set-window-configuration nav/elscreen-music-screen-layout))

(elscreen-start)
(nav/elscreen-init)
;; don't display the [X] icon
(setq elscreen-tab-display-kill-screen nil)
;; don's display the <-> icon
(setq elscreen-tab-display-control nil)
;; don't display the tabs anymore
(setq elscreen-display-tab nil)
(global-set-key (kbd "<C-s-right>") 'elscreen-next)
(global-set-key (kbd "<C-s-left>") 'elscreen-previous)
