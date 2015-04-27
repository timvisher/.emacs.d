(defun timvisher/add-my-public ()
  (interactive)
  (let* ((project-name (file-name-nondirectory (directory-file-name (magit-get-top-dir default-directory))))
         (remote       (format "git@github.com:timvisher/%s.git" project-name)))
    (magit-add-remote "public" remote)))

(setq magit-last-seen-setup-instructions "1.4.0")
