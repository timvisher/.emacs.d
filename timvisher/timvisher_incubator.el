(defun timvisher/dominating-compile ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory "Makefile")))
    (call-interactively 'compile)))

(defun timvisher/dominating-shell (root-file)
  (interactive "sfile: ")
  (let ((original-default-directory default-directory)
        (default-directory (locate-dominating-file default-directory root-file)))
    (if default-directory
        (call-interactively 'shell-command)
      (error (format "Could not locate `%s` above `%s`" root-file original-default-directory)))))
