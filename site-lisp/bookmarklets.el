(defun bookmarklets-bookmarklet-buffer-name ()
  (format "*bookmarklets-%s-bookmarklet*" (buffer-name (current-buffer))))

(defun bookmarklets-send-bookmarklet-buffer-to-clipboard ()
  (interactive)
  (with-current-buffer (bookmarklets-bookmarklet-buffer-name)
    (xclip-min-copy-buffer)))

(defun bookmarklets-make-bookmarklet-buffer ()
  (interactive)
  (let ((bookmarklet-buffer-name (bookmarklets-bookmarklet-buffer-name)))
    (kill-buffer bookmarklet-buffer-name)
    (let ((output-buffer (get-buffer-create bookmarklet-buffer-name)))
      (call-process-region (point-min) (point-max) "yui-compressor" nil output-buffer nil "--type" "js")
      (with-current-buffer output-buffer
        (goto-char (point-min))
        (insert "javascript:")
        (setq buffer-read-only t)
        (display-buffer output-buffer)))))

(provide 'bookmarklets)
