(defun timvisher/ack-from-root (arg)
  (interactive "P")
  (let ((current-prefix-arg (cond ((not arg) 4)

                                  ((= 4 (car arg)) 16)

                                  ((<= 16 (car arg)) nil))))
    (call-interactively 'ack)))

(autoload 'ack-command "ack")

(defun timvisher/isearch-ack-from-root (arg regexp)
  (interactive
   (list current-prefix-arg
         (cond
          ((functionp isearch-word)
           (funcall isearch-word isearch-string))
          (isearch-word (word-search-regexp isearch-string))
          (isearch-regexp isearch-string)
          (t (regexp-quote isearch-string)))))
  (let ((ack-command (concat "ag " regexp)))
    (isearch-done nil)
    (isearch-clean-overlays)
    (call-interactively 'timvisher/ack-from-root)))

(define-key isearch-mode-map (kbd "C-c r g") 'timvisher/isearch-ack-from-root)

;;; OOO, I wants it! http://www.youtube.com/watch?v=Wzr12gBrXA8
;; (define-key ack-minibuffer-local-map (kbd "C-w") 'isearch-yank-word-or-char)

;;; git grep is pretty awesome, so is ack
(global-unset-key (kbd "C-c r"))        ; Not sure what `C-c r` normally does but I don't like it!
;; (gloabl-set-key (kbd "C-c r g") 'vc-git-grep)
(global-set-key (kbd "C-c r g") 'timvisher/ack-from-root)
