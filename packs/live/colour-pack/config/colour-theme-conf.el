(live-add-pack-lib "color-theme")
(require 'color-theme)

;; use blackbored colour theme
(load (concat (live-pack-lib-dir) "cyberpunk"))
(load (concat (live-pack-lib-dir) "gandalf"))

(color-theme-cyberpunk)
(set-cursor-color "yellow")
