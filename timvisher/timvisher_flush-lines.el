(defun timvisher/isearch-flush-lines (regex)
  (interactive
   (list (cond
          ((functionp isearch-word)
           (funcall isearch-word isearch-string))
          (isearch-word (word-search-regexp isearch-string))
          (isearch-regexp isearch-string)
          (t (regexp-quote isearch-string)))))
  (save-excursion
    (flush-lines regex (point-min) (point-max))))

(define-key isearch-mode-map (kbd "C-c f") 'timvisher/isearch-flush-lines)
