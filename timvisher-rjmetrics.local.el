(load-theme 'solarized-light)

(require 'mdfind)

(require 'php-mode)

(defun timvisher/indent-tabs-mode () (setq indent-tabs-mode t))

(add-hook 'php-mode-hook 'timvisher/indent-tabs-mode)

(defvar timvisher/find-rjmetrics-file-project-dir nil)

(defun timvisher/find-rjmetrics-file (&optional arg)
  "Use projectile fuzzy find in a /core:/opt/code dir

C-u causes the parent project to be re-read.

C-u C-u causes the parent project to be re-read and the
projectile cache for that project to be cleared."
  (interactive "p")
  (when (or (= 4 arg)
            (= 16 arg)
            (not timvisher/find-rjmetrics-file-project-dir))
    (let* ((project-dirs      (cddr (directory-files "/core:/opt/code")))
           (choice            (ido-completing-read "Project: " project-dirs))
           (project-dir       (format "%s/%s" "/core:/opt/code" choice)))
      (setq timvisher/find-rjmetrics-file-project-dir project-dir)))
  (let ((default-directory timvisher/find-rjmetrics-file-project-dir))
    (projectile-find-file (= 16 arg))))

(global-set-key (kbd "C-c R") 'timvisher/find-rjmetrics-file)
