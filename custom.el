(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-mode-common-hook (quote (hs-minor-mode (lambda nil (dolist (k (quote (":" ">" ";" "<" "{" "}"))) (define-key (symbol-value (make-local-variable (quote yas/keymap))) k (quote self-insert-command)))))) t)
 '(deft-extension "md")
 '(deft-text-mode (quote markdown-mode))
 '(erc-email-userid "tim.visher@gmail.com")
 '(erc-nick "timvisher")
 '(erc-prompt-for-password t)
 '(erc-user-full-name "Tim Visher")
 '(inferior-lisp-program "lein repl")
 '(ispell-program-name "aspell")
 '(reb-re-syntax (quote string))
 '(sentence-end-double-space nil)
 '(slime-net-coding-system (quote utf-8-unix)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t (:inherit highlight))))
 '(widget-field ((t (:inherit highlight)))))
