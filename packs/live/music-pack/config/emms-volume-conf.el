(require 'emms-volume)

(defun nav/emms-channel-volume-change (channel amount)
  "Change amixer channel volume by AMOUNT."
  (message "Playback channel %s: %s"
           channel
           (with-temp-buffer
             (when (zerop
                    (call-process "amixer" nil (current-buffer) nil
                                  "sset" channel
                                  (format "%d%%%s" (abs amount)
                                          (if (< amount 0) "-" "+"))))
               (if (re-search-backward "\\[\\([0-9]+%\\)\\]" nil t)
                   (match-string 1))))))

(defun nav/emms-channel-volume-get (channel)
  (with-temp-buffer
    (when (zerop
           (call-process "amixer" nil (current-buffer) nil
                         "sget" channel))
      (if (re-search-backward "\\[\\([0-9]+\\)%\\]" nil t)
          (match-string 1)))))

(defun nav/emms-volume-change-fn (amount)
  (let ((master-vol (string-to-number (nav/emms-channel-volume-get "Master")))
        (preamp-vol (string-to-number (nav/emms-channel-volume-get "Pre-Amp"))))
    (cond ((and (= master-vol 100) (= preamp-vol 20) (< amount 0))
           (nav/emms-channel-volume-change "Master" amount))
          ((= master-vol 100) (nav/emms-channel-volume-change "Pre-Amp" amount))
          (t (nav/emms-channel-volume-change "Master" amount)))))

(setq emms-volume-change-function 'nav/emms-volume-change-fn)
