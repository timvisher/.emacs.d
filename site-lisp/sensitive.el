;;; sensitive.el --- A dead simple way to load sensitive information

;; This file is not part of GNU Emacs

;;; Commentary:

;; This package is intended to make it dead simple to include
;; sensitive information in your public .emacs file. More people
;; publishing their .emacs file is a good thing for the world of Emacs
;; users and the more facilities people have for hiding the
;; information they need to load without having to jump through hoops
;; to load it should make that easier.

;;; License:

;; [CC BY](http://creativecommons.org/licenses/by/3.0/)

;;; Contributors

;; Tim Visher ([@timvisher](https://twitter.com/timvisher))

;;; Code:

(require 'cl-lib)
(require 'sequences)

;;;###autoload
(defun load-sensitive-files ()
  (cl-dolist (setting-file (cl-remove-if 'file-directory-p (file-seq "~/Dropbox/sensitive")))
    (with-temp-buffer
      (insert-file-contents setting-file)
      (goto-char (point-min))
      (let ((package-name (intern (file-name-base (substring (file-name-directory setting-file) 0 -1))))
            (var-name     (intern (file-name-base setting-file)))
            (value        (read (current-buffer))))
        (message (format "Setting %s to %s after %s is loaded." (symbol-name var-name) value (symbol-name package-name) ))
        (eval-after-load package-name
          (set var-name value))))))

(provide 'sensitive)

;;; Local Variables:
;;; tab-width:2
;;; indent-tabs-mode:nil
;;; lexical-binding:t
;;; End:
;;; sensitive.el ends here
