(defun join (separator sequence)
  (apply 'concat
         (append (list (car sequence))
                 (mapcar (lambda (part)
                           (concat separator part))
                         (cdr sequence)))))

(provide 'string)
