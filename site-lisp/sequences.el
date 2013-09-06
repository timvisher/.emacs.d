(defun tree-seq (branch? children root)
  (cl-labels ((walk (node)
                    (cons node
                          (if (funcall branch? node)
                              (cl-mapcan #'walk (cddr (funcall children node)))))))
    (walk root)))

(defun file-seq (root)
  (tree-seq 'file-directory-p
            (lambda (directory)
              (directory-files directory t))
            (expand-file-name root)))

(provide 'sequences)
