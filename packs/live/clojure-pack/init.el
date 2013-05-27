;; Clojure Pack

(require 'rainbow-delimiters)

(live-add-pack-lib "uuid")
(require 'uuid)

(live-load-config-file "paredit-conf")
(live-load-config-file "mic-paren-conf")
(live-load-config-file "highlight-flash-conf")
(live-load-config-file "clojure-conf")
(live-load-config-file "auto-complete-conf")
(live-load-config-file "nrepl-conf")
;;(live-load-config-file "overtone-conf") TODO - fix for nrepl
