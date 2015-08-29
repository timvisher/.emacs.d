(load-theme 'solarized-light)

(require 'mdfind)

(require 'php-mode)

(defun timvisher/indent-tabs-mode () (setq indent-tabs-mode t))

(add-hook 'php-mode-hook 'timvisher/indent-tabs-mode)

;;; FIXME Main problem is that this breaks on tramp. Should be some
;;; way to disable it for tramp files only.
;; (turn-on-pbcopy)
