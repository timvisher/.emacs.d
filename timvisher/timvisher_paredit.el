(defun timvisher/map-custom-paredit-keys ()
  (message "timvisher/map-custom-paredit-keys has been called, CHARNOCK!")
  (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)
  (define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
  (define-key paredit-mode-map (kbd "}") 'paredit-close-curly)
  (define-key paredit-mode-map (kbd "[") 'paredit-open-square)
  (define-key paredit-mode-map (kbd "]") 'paredit-close-square)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-sexp)
  (message "timvisher/map-custom-paredit-keys has finished, CHARNOCK!"))

;;; Let's make paredit honor our awesome keys
(eval-after-load 'paredit
  '(timvisher/map-custom-paredit-keys))

(defun timvisher/add-paredit-to-mode-hooks ()
  (message "timvisher/add-paredit-to-mode-hooks sCHARNOCK")
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (message "timvisher/add-paredit-to-mode-hooks eCHARNOCK"))

(eval-after-load 'paredit
  '(timvisher/add-paredit-to-mode-hooks))

(eval-after-load 'paredit
  '(message "paredit has loaded CHARNOCK"))
