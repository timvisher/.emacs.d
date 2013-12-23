(defun timvisher/map-custom-paredit-keys ()
  (message "timvisher/map-custom-paredit-keys has been called, CHARNOCK!")
  (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)
  (define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
  (define-key paredit-mode-map (kbd "}") 'paredit-close-curly)
  (define-key paredit-mode-map (kbd "[") 'paredit-open-square)
  (define-key paredit-mode-map (kbd "]") 'paredit-close-square)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-sexp)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (message "timvisher/map-custom-paredit-keys has finished, CHARNOCK!"))

;;; Let's make paredit honor our awesome keys
(eval-after-load 'paredit
  '(timvisher/map-custom-paredit-keys))

(autoload 'enable-paredit-mode "paredit"
     "Turn on pseudo-structural editing of Lisp code."
     t)

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
