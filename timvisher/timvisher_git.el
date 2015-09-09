(defun timvisher/add-my-public ()
  (interactive)
  (let* ((project-name (file-name-nondirectory (directory-file-name (magit-toplevel default-directory))))
         (remote       (format "git@github.com:timvisher/%s.git" project-name)))
    (magit-remote-add "public" remote)))

(setq magit-last-seen-setup-instructions "1.4.0")

(defun github-source-link (universal-arg)
  "Constructs a link to the current file's github repo.

C-u will copy the link to the killring.
C-u C-u will attempt to open it in your default browser."
  (interactive "\p")
  (let* ((org-and-repo (s-split "/" (cadr (s-split ":" (magit-get "remote" (magit-get-remote) "url")))))
         (org          (car org-and-repo))
         (repo         (s-replace ".git" "" (cadr org-and-repo)))
         (branch       (magit-get-current-branch))
         (path         (magit-file-relative-name buffer-file-name))
         (url          (if (region-active-p)
                           (let ((beg-line (line-number-at-pos (region-beginning)))
                                 (end-line (line-number-at-pos (region-end))))
                             (format "https://github.com/%s/%s/blob/%s/%s#L%d-L%d"
                                     org
                                     repo
                                     branch
                                     path
                                     beg-line
                                     end-line))
                         (format "https://github.com/%s/%s/blob/%s/%s#L%d"
                                 org
                                 repo
                                 branch
                                 path
                                 (line-number-at-pos)))))
    (message url)
    (if (= 4 universal-arg)
        (kill-new url))
    (if (= 16 universal-arg)
        (browse-url url))))
