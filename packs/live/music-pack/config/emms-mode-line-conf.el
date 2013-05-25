(require 'nav-emms-mode-line)

;; Activate nav-emms-mode-line in all emms buffers.
;; this function has to go after elscreen-init, so append
(add-hook 'after-init-hook (lambda ()
                             (interactive)
                             (dolist (buffer (list emms-lyrics-buffer
                                                   emms-playlist-buffer
                                                   emms-browser-buffer))
                               (with-current-buffer buffer
                                 (nav/emms-mode-line 1)))) t)
