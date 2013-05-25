
;; Create tmp dirs and initialize dir vars
(load-file (concat user-emacs-directory "bootstrap-dirs.el"))

(defun live-version ()
  "1.0beta22")

;; load live-lib
(load-file (concat live-lib-dir "live-core.el"))

;;default packs
(let* ((pack-names '("foundation-pack"
                     "mail-pack"
                     "file-management-pack"
                     "colour-pack"
                     "clojure-pack"
                     "lang-pack"
                     "power-pack"
                     "git-pack"
                     "music-pack"
                     "bindings-pack"))
       (live-dir (file-name-as-directory "live"))
       (dev-dir  (file-name-as-directory "dev")))
  (setq live-packs (mapcar (lambda (p) (concat live-dir p)) pack-names) )
  (setq live-dev-pack-list (mapcar (lambda (p) (concat dev-dir p)) pack-names) ))

;; Load all packs - Power Extreme!
(mapcar (lambda (pack-dir)
          (live-load-pack pack-dir))
        (live-pack-dirs))
