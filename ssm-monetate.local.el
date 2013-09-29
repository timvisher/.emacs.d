;;; ----------------------------------------------------------------------------
;;; Let's set up the some system variables
;;; ----------------------------------------------------------------------------

(setq exec-path (split-string (getenv "PATH") ":"))
(setq exec-path (append (list (concat (getenv "HOME") "/.gem/ruby/1.8/bin"))
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

(setq exec-path (append (list (concat (getenv "HOME") "/.lein/bin")
                              (concat (getenv "HOME") "/bin")
                              (let ((brew-gnu-prefix (shell-command-to-string "brew --prefix coreutils")))
                                (concat (substring brew-gnu-prefix 0 (1- (length brew-gnu-prefix))) "/libexec/gnubin")))
                        exec-path))

(dolist (variable '("PATH" "EMACSPATH"))
  (setenv variable "")
  (dolist (value exec-path)
    (setenv variable
            (concat (getenv variable) value ":")))
  (setenv variable (substring (getenv variable) 0 -1)))

(setenv "JAVA_HOME" "/Library/Java/Home")

;;; ----------------------------------------------------------------------------
;;; Let's get our modes set up
;;; ----------------------------------------------------------------------------

;;; Growl from Emacs? Boom!
(if (featurep 'todochicku)
    (require 'todochiku))

;;; Why, oh why does brew insist on installing gnu tools at `g` prefixes?
(eval-after-load 'grep
  '(setq grep-find-template "gfind . <X> -type f <F> -exec grep <C> -nH <R> {} \\;"))

;;; System specific deft-directory location.
(eval-after-load "deft" '(setq deft-directory (concat (getenv "HOME") "/Dropbox/deft notes/")))

;;; ----------------------------------------------------------------------------
;;; Set up our custom functions
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; Start up default processes
;;; ----------------------------------------------------------------------------

(setq erc-nick "timvisher")

;;; Always run ERC on my mac.
;; (condition-case nil
;;     (erc :server "irc.freenode.net" :nick "timvisher")
;;   (error "Failed to connect to IRC!"))
