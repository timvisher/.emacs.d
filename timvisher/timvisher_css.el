(defun align-css-block ()
  (interactive)
  (save-excursion
   (er/mark-inside-pairs)
   (indent-region (region-beginning) (region-end))
   (align-regexp (region-beginning) (region-end) ":\\([[:space:]]*\\)" 1 1 t)))

(add-to-list 'auto-mode-alist '("\\.scss" . sass-mode))
