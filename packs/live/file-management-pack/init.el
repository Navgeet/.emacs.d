(live-load-config-file "sc-conf")
(live-load-config-file "dired-conf")

(require 'openwith)
(openwith-mode 1)

(setq openwith-associations
      '(("\\.pdf\\'" "acroread" (file))
        ("\\.\\(?:mp3\\|flac\\)\\'" "vlc" (file))
        ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\|mkv\\)\\'" "vlc" (file))))
