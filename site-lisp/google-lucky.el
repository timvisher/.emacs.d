;;; -*-lexical-binding:t;-*-
(require 'url)

(defun google-lucky-search (term callback)
  (url-retrieve (format "http://www.google.com/search?q=%s&btnI" (url-hexify-string term))
                (lambda (status)
                  (when (eq :redirect (car status))
                    (funcall callback (cadr status))))
                nil
                t))

(defun google-lucky-insert ()
  ;; Eventually make this able to decide whether or not to use the
  ;; region based on whether it's active. If it's not active, read
  ;; from a prompt
  ;; (interactive "Search for: ")
  (interactive)
  (if (region-active-p)
      (google-lucky-search (buffer-substring-no-properties (region-beginning) (region-end))
                           (let ((b (current-buffer)))
                             (lambda (url)
                               (with-current-buffer b
                                 (delete-region (region-beginning) (region-end))
                                 (insert url)))))
    (display-warning 'google-lucky "No active region" :warning)))

(defun google-lucky-open ()
  (interactive)
  (if (region-active-p)
      (google-lucky-search (buffer-substring-no-properties (region-beginning) (region-end))
                           (lambda (url)
                             (browse-url url)))
    (display-warning 'google-lucky "No active region" :warning)))

(global-set-key (kbd "S-l l") 'google-lucky-insert)
(global-set-key (kbd "S-l o") 'google-lucky-open)

(provide 'google-lucky)
