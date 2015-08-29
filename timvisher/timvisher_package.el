;;; ----------------------------------------------------------------------------
;;; Let's set up elpa, because it makes Emacs a more civilized place to live.
;;; ----------------------------------------------------------------------------

(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/")
;;              t)
;; (delete '("marmalade" . "http://marmalade-repo.org/packages/") package-archives)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (delete '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (delete '("melpa-stable" . "http://stable.melpa.org/packages/") package-archives)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar timvisher/my-packages '(ag
                                better-defaults
                                dash
                                dash-functional
                                deft
                                elisp-slime-nav
                                expand-region
                                ;; TODO Try helm out in a few months when it's a little more stable.
                                ;; helm
                                idle-highlight-mode
                                ido-ubiquitous
                                magit
                                markdown-mode
                                paredit
                                pbcopy
                                sensitive
                                smartparens
                                smex
                                textmate
                                ;; todochiku
                                vimgolf
                                wgrep
                                yasnippet))

(defun timvisher/install-package-list (package-list)
  (dolist (p package-list)
    (when (not (package-installed-p p))
      (package-install p))))

;; (timvisher/install-package-list timvisher/my-packages)

