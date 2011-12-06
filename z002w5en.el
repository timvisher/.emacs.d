(require 'cl)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/eclim")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/eclim/vendor")

;; (require 'eclim)

;; (setq eclim-auto-save t)
;; (global-eclim-mode)

(defun grails-run (args) "Run grails within a grails project."
  (interactive "MRun Grails with arguments: ")
  (let ((current-directory default-directory)
        (grails-home (locate-dominating-file default-directory "grails-app")))
    (progn
      (cd grails-home)
      (shell-command (concat "grails " args))
      (cd current-directory))))

(defun shell-explicit-bash ()
  "Start /bin/sh with `shell'."
  (interactive)
  (let ((shell-file-name "bash"))
    (shell (get-buffer-create "*shell-bash*"))))

(defun browse-url-ie ()
  "Browse url with ie"
  (interactive)
  (let ((browse-url-generic-program "iexplore"))
    (call-interactively 'browse-url-generic)))

(defun shell-bash ()
  "Create a new comint bash shell."
  (interactive)
  (let ((explicit-shell-file-name "c:/bin/bash"))
    (shell "*shell-bash*")))

(setq w32-lwindow-modifier 'super)
(setq w32-rwindow-modifier 'super)
(setq w32-pass-lwindow-to-system nil)
(setq w32-pass-rwindow-to-system nil)
(setq w32-apps-modifier 'hyper)

(set-register ?t '"//mlvv258a/temp")
(set-register ?c '(file . "d:/view_store/z002w5en_view"))

(eval-after-load 'textmate '(add-to-list '*textmate-project-roots* ".project"))

(shell)
