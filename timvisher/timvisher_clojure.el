(defvar timvisher/clojure-packages '(clojure-mode
                                     cider
                                     align-cljlet
                                     clojure-test-mode))

;; (timvisher/install-package-list timvisher/clojure-packages)

(defun timvisher/require-align-cljlet ()
  (require 'align-cljlet))

(add-hook 'clojure-mode-hook 'timvisher/require-align-cljlet)

;;; We use this to make it impossible to edit jar files by mistake.
(defun timvisher/make-read-only () (read-only-mode 1))

;;; Let's configure clojure-mode-hook with some extra goodness

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(autoload 'cider-turn-on-eldoc-mode "cider")

(eval-after-load 'clojure-mode
  '(add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode))

(autoload 'clojure-enable-cider "cider")

(eval-after-load 'clojure-mode
  '(add-hook 'clojure-mode-hook 'clojure-enable-cider))

(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "C-c l") 'align-cljlet))

(defun timvisher/add-paredit-to-clojure-mode-hook ()
  (unless (featurep 'paredit)
    (require 'paredit))
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

(eval-after-load 'clojure-mode
  '(timvisher/add-paredit-to-clojure-mode-hook))

(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)

(defun timvisher/add-midje-forms-to-clojure-dedenting ()
  (put-clojure-indent 'fact-group 1)
  (put-clojure-indent 'facts 1)
  (put-clojure-indent 'fact 1)
  (put-clojure-indent 'tabular nil)
  (put-clojure-indent 'for-all 1))

(eval-after-load 'clojure-mode
  '(add-hook 'clojure-mode-hook 'timvisher/add-midje-forms-to-clojure-dedenting))

(defun timvisher/add-compojure-forms-to-clojure-dedenting ()
  (put-clojure-indent 'context 2)
  (put-clojure-indent 'PUT 2)
  (put-clojure-indent 'GET 2)
  (put-clojure-indent 'POST 2)
  (put-clojure-indent 'DELETE 2)
  (put-clojure-indent 'PATCH 2))

(eval-after-load 'clojure-mode
  '(add-hook 'clojure-mode-hook 'timvisher/add-compojure-forms-to-clojure-dedenting))

(defun timvisher/add-test-check-forms-to-clojure-dedenting ()
  (put-clojure-indent 'quick-check 1))

(eval-after-load 'clojure-mode
  '(add-hook 'clojure-mode-hook 'timvisher/add-test-check-forms-to-clojure-dedenting))

