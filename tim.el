;; Startup

(setq exec-path (split-string (getenv "PATH") ":"))
(setq exec-path (append (list (concat (getenv "HOME") "/.lein/bin")
                              (concat (getenv "HOME") "/bin")
                              (concat (getenv "HOME") "/.gem/ruby/1.8/bin"))
                        (let ((brew-home "/usr/local"))
                          (list (concat brew-home "/bin")
                                (concat brew-home "/sbin")))
                        exec-path))

(dolist (variable '("PATH" "EMACSPATH"))
  (setenv variable "")
  (dolist (value exec-path)
    (setenv variable
            (concat (getenv variable) value ":")))
  (setenv variable (substring (getenv variable) 0 -1)))

(setenv "JAVA_HOME" "/Library/Java/Home")

(if (boundp 'mac-command-modifier) (setq mac-command-modifier 'meta))
(if (boundp 'mac-option-modifier) (setq mac-option-modifier 'super))
;; (if (boundp 'mac-control-modifier) (setq mac-control-modifier 'super))

(require 'cl)

;; Functions

(defun cradle-run (args)
  (interactive "MRun Cradle with args: ")
  (let ((current-directory default-directory)
        (cradle-home (locate-dominating-file default-directory "build.clj")))
    (progn
      (cd cradle-home)
      (shell-command (concat "cradle " args))
      (cd current-directory))))

(defun cradle-deploy ()
  (interactive)
  (cradle-run "deploy"))
