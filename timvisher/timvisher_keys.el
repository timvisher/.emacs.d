;;; <f1> should be the only way to access help, because C-h is better bound to backward-delete-char-untabify
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;;; I would think the following would suffice to have C-h do the right thing in smex but it appears not to.
(defun timvisher/fix-smex ()
  (define-key ido-completion-map (kbd "<f1> f") 'smex-describe-function)
  (define-key ido-completion-map (kbd "<f1> w") 'smex-where-is)
  (define-key ido-completion-map (kbd "C-h") 'delete-backward-char))

;; (eval-after-load 'ido
;;   '(eval-after-load 'smex
;;      '(timvisher/fix-smex)))

(define-key isearch-mode-map (kbd "<f1>") 'isearch-help-map)

