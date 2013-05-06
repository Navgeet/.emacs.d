;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap directory layout
;; Based on https://github.com/overtone/emacs-live/tree/70d8a908ae
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Store live base dirs, but respect user's choice of `live-root-dir'
;; when provided.
(setq live-root-dir (if (boundp 'live-root-dir)
                        (file-name-as-directory live-root-dir)
                      user-emacs-directory))

(setq
 live-tmp-dir      (file-name-as-directory (concat live-root-dir "tmp"))
 live-etc-dir      (file-name-as-directory (concat live-root-dir "etc"))
 live-pscratch-dir (file-name-as-directory (concat live-tmp-dir  "pscratch"))
 live-lib-dir      (file-name-as-directory (concat live-root-dir "lib"))
 live-packs-dir    (file-name-as-directory (concat live-root-dir "packs"))
 live-autosaves-dir(file-name-as-directory (concat live-tmp-dir  "autosaves"))
 live-backups-dir  (file-name-as-directory (concat live-tmp-dir  "backups"))
 live-load-pack-dir nil)

;; create tmp dirs if necessary
(make-directory live-etc-dir t)
(make-directory live-tmp-dir t)
(make-directory live-autosaves-dir t)
(make-directory live-backups-dir t)
(make-directory live-pscratch-dir t)
