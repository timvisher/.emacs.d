(defvar timvisher/clojure-packages '(clojure-mode
                                     cider
                                     align-cljlet
                                     clojure-test-mode
                                     elein))

;; (timvisher/install-package-list timvisher/clojure-packages)

(autoload 'align-cljlet "align-cljlet")

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

(defun timvisher/turn-on-elein ()
  (unless (featurep 'elein)
    (require 'elein)))

(eval-after-load 'clojure-mode
  '(add-hook 'clojure-mode-hook 'timvisher/turn-on-elein))

(defun timvisher/add-paredit-to-clojure-mode-hook ()
  (unless (featurep 'paredit)
    (require 'paredit))
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

(eval-after-load 'clojure-mode
  '(timvisher/add-paredit-to-clojure-mode-hook))
