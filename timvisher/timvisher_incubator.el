(autoload 'vc-deduce-backend "vc")

(defun timvisher/dominating-compile ()
  (interactive)
  (let ((default-directory (vc-call-backend (vc-deduce-backend) 'root default-directory)))
    (call-interactively 'compile)))

(defun timvisher/dominating-shell (root-file)
  (interactive "sfile: ")
  (let ((original-default-directory default-directory)
        (default-directory (locate-dominating-file default-directory root-file)))
    (if default-directory
        (call-interactively 'shell-command)
      (error (format "Could not locate `%s` above `%s`" root-file original-default-directory)))))

(defmacro timvisher/with-package-repos (package-repo-urls &rest body)
  (declare (indent 1))
  `(let* ((package-repos (mapcar (lambda (package-repo-url)
                                   `(,(secure-hash 'md5 package-repo-url)
                                     .
                                     ,package-repo-url))
                                 ,package-repo-urls))
          (package-archives package-repos))
     ,@body))

;; (timvisher/with-package-repos '("http://marmalade-repo.org/packages/" "http://melpa.milkbox.net/packages/")
;;   package-archives
;;   (package-list-packages))

(defun replace-with-solved-equation (start-of-equation end-of-equation)
  (interactive "r")
  (if (not (region-active-p))
      (error "Requires an active region"))

  (let ((equation-string (buffer-substring start-of-equation end-of-equation)))
    (calc-embedded nil)
    (calc-embedded nil)
    (save-excursion
      (goto-char start-of-equation)
      (insert equation-string " = "))))

(defun github-source-link (open)
  "Constructs a link to the current file's github repo.

C-u will attempt to open it in your default browser."
  (interactive "\P")
  (let* ((org-and-repo (s-split "/" (cadr (s-split ":" (magit-get "remote" (magit-get-current-remote) "url")))))
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
    (if open
        (browse-url url))))

(defun align-vector-of-vectors-elements ()
  "aligns the current vector of vector's elements

Point must be in the parent vector but not in any of the child
vectors when called."
  (interactive)
  (save-excursion
    (paredit-backward-up)
    (if (not (looking-at "\\["))
        (error "Must be in a vector."))
    (paredit-forward-down)
    (if (not (looking-at "\\["))
        (error "Must be in a vector."))
    (er/mark-inside-pairs)
    (let ((beg (region-beginning))
          (end (region-end)))
      (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\"" 1 1 nil)
      (while (search-forward-regexp "^ +[^ []" (region-end) t)
        (join-line)
        (goto-char beg)
        (er/mark-inside-pairs))
      (newline)
      (indent-region (region-beginning) (region-end))
      (paredit-backward-up)
      (paredit-forward)
      (paredit-backward-down)
      (newline)
      (er/mark-inside-pairs)
      (beginning-of-line)
      (sort-lines nil (point) (region-end))
      (paredit-backward-up)
      (paredit-forward-down)
      (paredit-forward)
      (join-line)
      (paredit-backward-up)
      (paredit-forward)
      (join-line))))

;;; Write align-ns-form
;;; look-at each :require and align-regexp ":"
;;; join-line for whitespace with no leading ( or [
