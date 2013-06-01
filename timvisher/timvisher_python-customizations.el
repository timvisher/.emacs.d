(defun timvisher/python-nav-backward-sexp ()
  (interactive)
  (python-nav--backward-sexp))

(defun timvisher/map-custom-python-mode-keys ()
  (define-key python-mode-map (kbd "C-M-f") 'python-nav-forward-sexp)
  (define-key python-mode-map (kbd "C-M-b") 'timvisher/python-nav-backward-sexp)
  (define-key python-mode-map (kbd "C-M-d") 'python-nav-up-list)
  (define-key python-mode-map (kbd "C-M-u") 'python-nav-backward-up-list))

(eval-after-load 'python
  '(timvisher/map-custom-python-mode-keys))

