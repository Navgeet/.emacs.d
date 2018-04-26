(require 'whitespace)

(setq whitespace-line-column 80
      whitespace-style '(face
                         ;; trailing lines-tail indentation
                         ;; trailing indentation
                         space-before-tab space-after-tab))

(global-whitespace-mode 1)
