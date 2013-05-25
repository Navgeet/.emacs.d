;; Set up EMMS
(live-add-pack-lib "emms/lisp")

(require 'emms-setup)
(emms-standard)
(emms-default-players)

(defun emms-stop-and-next ()
  (interactive)
  (emms-stop)
  (emms-next)
  (emms-stop))

(defvar nav/emms-currently-playing-track nil)

(defcustom nav/emms-player-started-hook nil
  "*Hook run when an EMMS player starts playing."
  :group 'emms
  :type 'hook
  :options '(emms-show))

(let ((func (lambda ()
	      (interactive)
	      (setq nav/emms-currently-playing-track nil))))
  (add-hook 'emms-player-stopped-hook func)
  (add-hook 'emms-player-finished-hook func))

;;; Patches
(defun emms-player-start (track)
  "Start playing TRACK."
  (if emms-player-playing-p
      (error "A player is already playing")
    (let ((player (emms-player-for track)))
      (if (not player)
          (error "Don't know how to play track: %S" track)
        ;; Change default-directory so we don't accidentally block any
        ;; directories the current buffer was visiting.
        (let ((default-directory "/"))
          (funcall (emms-player-get player 'start)
                   track)
	  (run-hook-with-args 'nav/emms-player-started-hook track))))))
