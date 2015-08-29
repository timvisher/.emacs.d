(load-theme 'solarized-light)

(require 'mdfind)

(require 'php-mode)

(defun timvisher/indent-tabs-mode () (setq indent-tabs-mode t))

(add-hook 'php-mode-hook 'timvisher/indent-tabs-mode)
