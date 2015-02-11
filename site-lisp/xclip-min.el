
(defun xclip-min-copy-region ()
  (interactive)
  (call-process-region (region-beginning)
                       (region-end)
                       "xclip"
                       nil
                       nil
                       nil
                       "-selection"
                       "clipboard")
  (message "Copied %s to clipboard."
           (buffer-substring-no-properties (region-beginning) (region-end))))

(defun xclip-min-copy-buffer ()
  (interactive)
  (call-process-region (point-min)
                       (point-max)
                       "xclip"
                       nil
                       nil
                       nil
                       "-selection"
                       "clipboard")
  (message "Copied %s to clipboard."
           (buffer-name (current-buffer))))

(defun xclip-min-paste ()
  (interactive)
  (call-process "xclip" nil t nil "-selection" "clipboard" "-o"))

(provide 'xclip-min)
