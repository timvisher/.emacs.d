(defvar timvisher/clojure-packages '(clojure-mode
                                     nrepl
                                     align-cljlet
                                     clojure-test-mode
                                     elein))

;; (timvisher/install-package-list timvisher/clojure-packages)

(autoload 'align-cljlet "align-cljlet")

;;; We use this to make it impossible to edit jar files by mistake.
(defun timvisher/make-read-only () (read-only-mode 1))

