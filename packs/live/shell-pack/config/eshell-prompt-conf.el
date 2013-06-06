(add-hook 'eshell-mode-hook (lambda ()
                              (interactive)
                              (setq header-line-format
                                    (list " "
                                          '(:eval (propertize (eshell/pwd)
                                                              'face 'mode-line))))))

(set-face-background 'header-line "gray10")

(setq eshell-prompt-function (lambda () "> "))
(setq eshell-prompt-regexp "> ")
