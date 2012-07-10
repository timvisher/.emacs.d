;;; ----------------------------------------------------------------------------
;;; Let's set the system up
;;; ----------------------------------------------------------------------------

(setenv "GRADLE_OPTS" (concat "-Dhttp.proxyHost=165.226.204.104 "
                              "-Dhttp.proxyPort=8080 "
                              "-Dhttp.nonProxyHosts=ml1002pc "
                              "-Dorg.gradle.daemon=false "
                              "-Xmx1500m "
                              "-XX:MaxPermSize=1500m"))

(setq w32-lwindow-modifier 'super)
(setq w32-rwindow-modifier 'super)
(setq w32-pass-lwindow-to-system nil)
(setq w32-pass-rwindow-to-system nil)
(setq w32-apps-modifier 'hyper)

(set-register ?t '"//mlvv258a/temp")
(set-register ?c '(file . "d:/view_store/z002w5en_view"))
(set-register ?~ '(file . "d:/view_store/z002w5en"))
(set-register ?w '(file . "c:/Documents and Settings/z002w5en"))

;;; ----------------------------------------------------------------------------
;;; My modes, I wants them!
;;; ----------------------------------------------------------------------------

(require 'w32-browser)

;;; TODO Wouldn't it be great to play with eclim one day?
;; (require 'eclim)

;; (setq eclim-auto-save t)
;; (global-eclim-mode)

(eval-after-load 'textmate '(add-to-list '*textmate-project-roots* ".project"))
(eval-after-load 'deft '(setq deft-directory (concat (getenv "HOME") "/Dropbox/deft notes/")))
(eval-after-load 'markdown-mode '(setq markdown-command "perl d:/view_store/z002w5en/bin/markdown"))
(eval-after-load 'locate '(setq locate-command "es"))

;;; ----------------------------------------------------------------------------
;;; Let's set up our custom functions
;;; ----------------------------------------------------------------------------

(defun grails-run (args) "Run grails within a grails project."
  (interactive "MRun Grails with arguments: ")
  (let ((current-directory default-directory)
        (grails-home (locate-dominating-file default-directory "grails-app")))
    (progn
      (cd grails-home)
      (shell-command (concat "grails " args))
      (cd current-directory))))

(defun browse-url-ie ()
  "Browse url with ie"
  (interactive)
  (let ((browse-url-generic-program "iexplore"))
    (call-interactively 'browse-url-generic)))
