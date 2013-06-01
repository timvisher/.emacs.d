;;; ----------------------------------------------------------------------------
;;; Let's set up elpa, because it makes Emacs a more civilized place to live.
;;; ----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

  ;; align-cljlet       0.3          installed  Space align various Clojure forms 
  ;; applescript-mode   1.1          installed  major mode for editing AppleScript source
  ;; centered-cursor... 0.5.1        installed  cursor stays vertically centered
  ;; cljdoc             0.1.0        installed  eldoc mode for clojure
  ;; clojure-mode       2.1.0        installed  Major mode for Clojure code
  ;; clojure-test-mode  2.1.0        installed  Minor mode for Clojure tests
  ;; clojurescript-mode 0.5          installed  Major mode for ClojureScript code
  ;; csv-mode           1.50         installed  major mode for editing comma-separated value files
  ;; dash               1.1.0        installed  A modern list library for Emacs
  ;; deft               0.3          installed  quickly browse, filter, and edit plain text notes
  ;; elein              0.2.2        installed  running leiningen commands from emacs
  ;; elisp-slime-nav    0.3          installed  Make M-. and M-, work in elisp like they do in slime
  ;; ercn               1.0.2        installed  Flexible ERC notifications
  ;; expand-region      0.8.0        installed  Increase selected region by semantic units.
  ;; find-file-in-pr... 3.2          installed  Find files in a project quickly.
  ;; furl               0.0.2        installed  Friendly URL retrieval
  ;; gnugo              2.2.12       installed  Play a game of Go against gnugo
  ;; idle-highlight-... 1.1.2        installed  highlight the word the point is on
  ;; ido-ubiquitous     1.6          installed  Use ido (nearly) everywhere.
  ;; magit              1.2.0        installed  Control Git from Emacs.
  ;; markdown-mode      1.9          installed  Emacs Major mode for Markdown-formatted text files
  ;; maxframe           0.5.1        installed  maximize the emacs frame based on display size
  ;; nrepl              0.1.7        installed  Client for Clojure nREPL
  ;; paredit            22           installed  minor mode for editing parentheses  -*- Mode: Emacs-Lisp -*-
  ;; slime              20100404.1   installed  Superior Lisp Interaction Mode for Emacs
  ;; smex               2.0          installed  M-x interface with Ido-style fuzzy matching.
  ;; starter-kit        2.0.3        installed  Saner defaults and goodies.
  ;; starter-kit-bin... 2.0.2        installed  Saner defaults and goodies: bindings
  ;; starter-kit-eshell 2.0.3        installed  Saner defaults and goodies: eshell tweaks
  ;; starter-kit-lisp   2.0.3        installed  Saner defaults and goodies for lisp languages
  ;; textmate           5            installed  TextMate minor mode for Emacs
  ;; todochiku          20120202     installed  A mode for interfacing with Growl, Snarl, and the like. [source: wiki]
  ;; vimgolf            0.9.2        installed  VimGolf interface for the One True Editor
  ;; wgrep              2.1.3        installed  Writable grep buffer and apply the changes to files

(defvar timvisher/my-packages '(htmlize
                                expand-region
                                deft
                                elisp-slime-nav
                                furl
                                idle-highlight-mode
                                ido-ubiquitous
                                magit
                                markdown-mode
                                maxframe
                                smex
                                paredit
                                find-file-in-project
                                starter-kit
                                starter-kit-bindings
                                starter-kit-eshell
                                starter-kit-lisp
                                textmate
                                ;; todochiku
                                ;; TODO Try helm out in a few months when it's a little more stable.
                                ;; helm
                                wgrep
                                vimgolf))

(defun timvisher/install-package-list (package-list)
  (dolist (p package-list)
    (when (not (package-installed-p p))
      (package-install p))))

(timvisher/install-package-list timvisher/my-packages)

