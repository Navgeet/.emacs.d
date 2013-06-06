;; define these before loading eshell
(defface eshell-ls-directory '((((background dark)) (:foreground "DodgerBlue")))
  "")
(defface eshell-ls-executable '((((background dark)) (:foreground "GreenYellow")))
  "")
(defface eshell-ls-clutter '((((background dark)) (:foreground "gray20")))
  "")
(defface eshell-ls-normal '((((background dark)) (:foreground "gray40")))
  "")

(require 'eshell)

(live-load-config-file "em-smart-conf")
(live-load-config-file "ansi-color-conf")
(live-load-config-file "eshell-color-conf")
(live-load-config-file "pcomplete-conf")
