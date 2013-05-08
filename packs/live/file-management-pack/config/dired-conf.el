(live-add-pack-lib "dired-details+")
(require 'dired-details+)
(require 'dired+)

;; Redefine some ugly faces in dired+.el
(setq diredp-file-name '((((background dark)) (:foreground "gray40"))
                         (t                   (:foreground "Blue"))))

(setq diredp-file-suffix '((((background dark)) (:foreground "gray60"))
                           (t                   (:foreground "DarkMagenta"))))

(setq diredp-ignored-file-name
      '((((background dark)) (:foreground "gray20")) ; ~ salmon
        (t                   (:foreground "#00006DE06DE0"))))

(setq diredp-dir-priv
      '((((background dark)) (:foreground "DodgerBlue"))
        (t (:foreground "DarkRed"))))

(setq diredp-symlink
    '((((background dark)) (:foreground "cyan"))
      (t                   (:foreground "DarkOrange"))))

(setq dired-listing-switches (purecopy "-lF"))
