;;; ----------------------------------------------------------------------------
;;; Let's set up elpa, because it makes Emacs a more civilized place to live.
;;; ----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
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

(defvar timvisher/my-packages '(clojure-mode
                                nrepl
                                align-cljlet
                                clojure-test-mode
                                htmlize
                                expand-region
                                deft
                                elein
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

(dolist (p timvisher/my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; ----------------------------------------------------------------------------
;;; And now let's get some system wide variables set up
;;; ----------------------------------------------------------------------------

;;; <f1> should be the only way to access help, because C-h is better bound to backward-delete-char-untabify
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;;; You always wants your custom info files to be in the Info-directory-list
(eval-after-load 'info 
  '(add-to-list 'Info-directory-list (concat (getenv "HOME") "/.emacs.d/info")))

;;; We need site-lisp to be here for stuff that hasn't made it out of the wilderness and into elpa
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;; ----------------------------------------------------------------------------
;;; Alright, time's up. Let's do enabling and configuring modes.
;;; ----------------------------------------------------------------------------

(electric-pair-mode 1)
(electric-indent-mode 1)

;;; Supposedly I need to turn this on to get scroll wheel support (like gnome-terminal) in iTerm but it also captures the mouse for things like cursor selection which breaks iterm selection clipboard copying. Not sure if I like this.
;; (xterm-mouse-mode 1)
;; (global-set-key (kbd "<mouse-4>") 'next-line)
;; (global-set-key (kbd "<mouse-5>") 'previous-line)

(autoload 'align-cljlet "align-cljlet")

(defun timvisher/make-read-only () (toggle-read-only 1))

;; ;;; If we really, really don't want ido-mode, this is how to do it!
;; (eval-after-load 'starter-kit-misc
;;   '(ido-mode nil))
;; (eval-after-load 'starter-kit-misc
;;   '(ido-ubiquitous nil))

(global-set-key (kbd "C-c k") 'kill-whole-line)

(defalias 'csr 'cua-set-rectangle-mark)

(defun timvisher/ack-from-root (arg)
  (interactive "P")
  (let ((current-prefix-arg (cond ((not arg) 4)

                                  ((= 4 (car arg)) 16)

                                  ((<= 16 (car arg)) nil))))
    (call-interactively 'ack)))

;;; OOO, I wants it! http://www.youtube.com/watch?v=Wzr12gBrXA8
;; (define-key ack-minibuffer-local-map (kbd "C-w") 'isearch-yank-word-or-char)

;;; git grep is pretty awesome, so is ack
(global-unset-key (kbd "C-c r"))        ; Not sure what `C-c r` normally does but I don't like it!
;; (gloabl-set-key (kbd "C-c r g") 'vc-git-grep)
(global-set-key (kbd "C-c r g") 'timvisher/ack-from-root)

;;; All the cool kids swap meta and super.
(if (boundp 'mac-command-modifier) (setq mac-command-modifier 'meta))
(if (boundp 'mac-option-modifier) (setq mac-option-modifier 'super))

;;; Make system and user specific emacs temp files
(setq eshell-history-file-name (concat (getenv "HOME") "/.emacs.d/eshell/" system-name "-history"))

;;; When [mickey][redisplay-dont-pause] tells you something will make Emacs feel faster, you should probably shut up and do it.
;; (setq redisplay-dont-pause t)

;;; Goal column rocks
(put 'set-goal-column 'disabled nil)

;;; All the cool kids use narrow-to-region
(put 'narrow-to-region 'disabled nil)

;;; upcase-region is too useful to leave disabled
(put 'upcase-region 'disabled nil)

(require 'dired-x) ; Ooo baby, Virtual Dired is the new hotness!

;;; Let's make sending mail just work exactly how you'd expect it to just work
(setq send-mail-function 'mailclient-send-it)
(setq message-send-mail-function 'message-send-mail-with-mailclient)

;;; @jwiegely says winner mode is 'da bomb so it seems like it would be worth using
(eval-after-load 'winner
  '(global-set-key (kbd "C-c [") 'winner-undo))
(eval-after-load 'winner
  '(global-set-key (kbd "C-c ]") 'winner-redo))

;;; markdown
;;; Let's fix `C-j` in markdown mode.
;;; TODO alias eval-after-load to eal
(eval-after-load "markdown"
  '(define-key markdown-mode-map (kbd "C-j") 'markdown-enter-key))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))

;;; Magnar Sven's mother was the golden goose: http://emacsrocks.com/
(global-set-key (kbd "C-c r =") 'er/expand-region)
(autoload 'er/contract-region "expand-region")
(global-set-key (kbd "C-c r -") 'er/contract-region)
(autoload 'er/mark-inside-quotes "expand-region")
(global-set-key (kbd "C-c r i \"") 'er/mark-inside-quotes)
(global-set-key (kbd "C-c r i \'") 'er/mark-inside-quotes)
(autoload 'er/mark-inside-pairs "expand-region")
(global-set-key (kbd "C-c r i p") 'er/mark-inside-pairs)
(autoload 'er/mark-outside-quotes "expand-region")
(global-set-key (kbd "C-c r a \"") 'er/mark-outside-quotes)
(global-set-key (kbd "C-c r a \'") 'er/mark-outside-quotes)
(autoload 'er/mark-outside-pairs "expand-region")
(global-set-key (kbd "C-c r a p") 'er/mark-outside-pairs)
(autoload 'er/mark-inner-tag "expand-region")
(global-set-key (kbd "C-c r i t") 'er/mark-inner-tag)
(autoload 'er/mark-outer-tag "expand-region")
(global-set-key (kbd "C-c r a t") 'er/mark-outer-tag)


;;; Ruby

(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

;;; ibuffer is the way to go for browsing your buffers!

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; TODO Try helm out in a few months when it's a little more stable.
;; ;;; helm is the bee's knees
;; (require 'helm-config)
;; ;;; TODO Play with helm-ls-git. Right now it doesn't seem to list every file available to git, which is the main reason it should be able to replace textmate-goto-file
;; (require 'helm-ls-git)
;; ;; (eval-after-load 'textmate
;; ;;   '(define-key *textmate-mode-map* [(super t)] 'helm-ls-git-ls))

;; ;;; Could there be a better way than helm-buffers-list to switch buffers?
;; (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)

;; ;;; And boy, oh boy! helm-find-files is pretty sweet!
;; (global-set-key (kbd "C-x f") 'helm-find-files)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

;; ;;; helm-ucs? Yes please!
;; (global-set-key (kbd "C-x 8 RET") 'helm-ucs)
;; (global-set-key (kbd "C-c 8") 'helm-ucs)

;; ;;; Let's not be crazy and use regular `M-x` now.
;; (global-set-key (kbd "M-x") 'helm-M-x)

;;; Let [Yegge][] tell the truth about what should be bound to delete.
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)

(defun timvisher/map-custom-paredit-keys ()
  (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)
  (define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
  (define-key paredit-mode-map (kbd "}") 'paredit-close-curly)
  (define-key paredit-mode-map (kbd "[") 'paredit-open-square)
  (define-key paredit-mode-map (kbd "]") 'paredit-close-square)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-sexp))

;;; Let's make paredit honor our awesome keys
(eval-after-load 'paredit
  '(timvisher/map-custom-paredit-keys))

;;; Let's configure clojure-mode-hook with some extra goodness

;;; Don't be stupid and forget that `clojure-jack-in` is how you
;;; should enable clojure-mode on your stuff
;; (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(eval-after-load 'clojure-mode
  '(add-hook 'nrepl-interaction-mode 'nrepl-turn-on-eldoc-mode))

(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "C-c l") 'align-cljlet))

(eval-after-load 'clojure-mode
  '(add-hook 'clojure-mode-hook 'timvisher/turn-on-elein))

(defun timvisher/turn-on-elein ()
  (unless (featurep 'elein)
    (require 'elein)))

(defun timvisher/turn-on-clojure-test () (clojure-test-mode 1))

;;; Now let's fix our slime

;;; We need to require clojure-test-mode explicitly because the version from elpa requires slime and messes with clojure-jack-in
(eval-after-load 'slime
  '(require 'clojure-test-mode))

(eval-after-load 'slime
  '(add-hook 'clojure-mode-hook 'timvisher/turn-on-clojure-test))

(eval-after-load 'slime
  '(timvisher/fix-slime-repl))

(defun timvisher/fix-slime-repl-lisp-indent-function () (setq lisp-indent-function 'clojure-indent-function))

(defun timvisher/fix-slime-repl-syntax-table () (set-syntax-table clojure-mode-syntax-table))

(defun timvisher/turn-on-paredit () (paredit-mode 1))

(defun timvisher/fix-slime-repl ()
  (add-hook 'slime-repl-mode-hook 'timvisher/fix-slime-repl-lisp-indent-function)
  (add-hook 'slime-repl-mode-hook 'timvisher/fix-slime-repl-syntax-table)
  (add-hook 'slime-repl-mode-hook 'timvisher/turn-on-paredit))

(eval-after-load 'textmate
  '(define-key *textmate-mode-map* [(super shift t)] 'helm-imenu))

;;; Looky, looky, I've got hooky
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'timvisher/turn-on-textmate-mode)
(remove-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(defun timvisher/turn-on-textmate-mode ()
  (textmate-mode 1))

(add-hook 'applescript-mode-hook 'run-prog-mode-hook)

(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'whitespace-mode)

(add-hook 'markdown-mode-hook 'visual-line-mode)

;;; ----------------------------------------------------------------------------
;;; What needs to be overridden shall be overridden
;;; ----------------------------------------------------------------------------

;;; Why is kmacro-edit-lossage so suckily implemented?
(defun kmacro-edit-lossage ()
  "Edit most recent 300 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro 'view-lossage))

;;; Make re-builder copy just the text without the `"` characters.
(defun reb-copy ()
  "Copy current RE into the kill ring for later insertion."
  (interactive)

  (reb-update-regexp)
  (let ((re (with-output-to-string
              (print (reb-target-binding reb-regexp)))))
    (kill-new (substring re 2 (- (length re) 2)))
    (message "Regexp copied to kill-ring")))

;;; ----------------------------------------------------------------------------
;;; Now, let's make Emacs our own.
;;; ----------------------------------------------------------------------------

;; (defun timvisher/find-file-in-git-project ()
;;   (interactive)
;;   (let ((current-directory default-directory)
;;         (git-root (expand-file-name (locate-dominating-file default-directory ".git"))))
;;     (cd git-root)
;;     (helm-ls-git-ls)
;;     (cd current-directory)))
;; (define-key *textmate-mode-map* [(super t)] 'timvisher/find-file-in-git-project)

(defun timvisher/edit-init-file ()
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c i") 'timvisher/edit-init-file)

(defun timvisher/clojure-test-comment ()
  (interactive)
  (beginning-of-defun)
  (search-forward " ")
  (let ((b (point)))
    (search-forward " ")
    (backward-char)
    (copy-region-as-kill b (point)))
  (end-of-defun)
  (open-line 1)
  (newline-and-indent)
  (insert "(comment
  (")
  (yank)
  (insert " )
)")
  (backward-char 3))

(defun timvisher/clojure-test-comment-in-comment-block-p ()
  (save-excursion
    (beginning-of-defun)
    (let ((cp (point)))
      (search-forward "(comment" nil "end")
      (= 8 (- (point) cp)))))

(defun timvisher/clear-top-level-form ()
  (beginning-of-defun)
  (let ((cp (point)))
    (end-of-defun)
    (next-line)
    (delete-region cp (point))))

(setq timvisher/function-forms '("defn" "defn-" "defmethod"))

(defun timvisher/clojure-at-function-definition-p ()
  (save-excursion
    (let ((cp (point)))
      )))

(defun timvisher/clojure-in-function-form-p ()
  (save-excursion
    (beginning-of-defun)))

(defun timvisher/clojure-test-comment ()
  (interactive)
  (if (not (timvisher/clojure-test-comment-in-comment-block-p)) (timvisher/clojure-insert-test-comment-call) (progn (timvisher/clear-top-level-form) (beginning-of-defun))))

(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "C-c c t c") 'timvisher/clojure-test-comment))

;;; Wouldn't it be so awesome if growl notified us when we were mentioned?

(defun timvisher/notify-of-mention (match-type nickuserhost message)
  (todochiku-message nickuserhost message (cdr (assoc 'irc todochiku-icons))))

;; (let ((current-deft-filter-regexp deft-filter-regexp))
;;     (if deft-filter-regexp
;;         (progn
;;           (deft-filter-clear)
;;           (deft-new-file)
;;           (deft-filter current-deft-filter-regexp))
;;       (deft-new-file)))

;; (defun timvisher/blog (blog-title)
;;   "Grab a new deft file and populate it with a blog entry titled BLOG-TITLE for right now."
;;   (interactive "sBlog Title: ")
;;   ;; (let ((current-deft-filter-regexp deft-filter-regexp))
;;   ;;   (if deft-filter-regexp
;;   ;;       (progn
;;   ;;         (deft-filter-clear)
;;   ;;         (deft-new-file)
;;   ;;         (deft-filter current-deft-filter-regexp))
;;   ;;     (deft-new-file)))
;;   (deft-new-file)
;;   (visual-line-mode 1)
;;   (insert "blogpost-"
;;           (format-time-string "%Y%m%d%H%M%S")
;;           "-"
;;           (replace-regexp-in-string " " "-" (downcase blog-title))
;;           "

;; # "
;;           (titlecase-string blog-title)
;;           "

;; ")
;;   (local-set-key (kbd "C-c C-q") 'delete-frame))

(eval-after-load "deft"
  '(defun timvisher/blog (blog-title)
     "Grab a new deft file and populate it with a blog entry titled BLOG-TITLE for right now."
     (interactive "sBlog Title: ")
     ;; (let ((current-deft-filter-regexp deft-filter-regexp))
     ;;   (if deft-filter-regexp
     ;;       (progn
     ;;         (deft-filter-clear)
     ;;         (deft-new-file)
     ;;         (deft-filter current-deft-filter-regexp))
     ;;     (deft-new-file)))
     (deft-new-file)
     (visual-line-mode 1)
     (insert "blogpost-"
             (format-time-string "%Y%m%d%H%M%S")
             "-"
             (replace-regexp-in-string " " "-" (downcase blog-title))
             "

# "
             (titlecase-string blog-title)
             "

")
     (local-set-key (kbd "C-c C-q") 'delete-frame)))

(eval-after-load "deft"
  '(require 'titlecase))

;;; Deft is pretty awesome, and my journal entries should probably be kept in it
(eval-after-load "deft"
  '(defun timvisher/journal ()
     "Grab a new deft file and populate it with a journal entry for right now"
     (interactive)
     (select-frame-set-input-focus (make-frame))
     (deft-new-file)
     (visual-line-mode 1)
     (insert "journal entry " (format-time-string "%Y%m%d%H%M%S") "

")
     (local-set-key (kbd "C-c C-q") 'delete-frame)))

;;; Let's put our edit clipboard functionality into deft as well
(eval-after-load "deft"
  '(defun timvisher/kill-ring-deft ()
     "Make a new deft file and yank the kill ring into it"
     (interactive)
     (select-frame-set-input-focus (make-frame))
     (deft-new-file)
     (visual-line-mode 1)
     (yank)
     (goto-char (point-min))
     (insert "

")
     (goto-char (point-min))
     (local-set-key (kbd "C-c C-q") 'timvisher/copy-buffer-and-kill-frame)))

(eval-after-load 'deft
  '(defalias 'mdf 'timvisher/kill-ring-deft))

(defun timvisher/copy-buffer-and-kill-frame ()
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (delete-frame))

;;; We need to restart jetty often enough that it's be worth making it into a function
(defun timvisher/lein-server ()
  (interactive)
  (let* ((process-name "lein-server")
         (process (get-process process-name))
         (current-directory default-directory)
         (lein-home (locate-dominating-file default-directory "project.clj")))
    (progn
      (if process (delete-process process))
      (cd lein-home)
      (start-process process-name "*lein server*" "lein" "ring" "server")
      (cd current-directory))))

;;; Ooo, that Eclipse sure does have some fancy-schmancy drag up and down features. Let's get that in Emacs as well.
;;; TODO Wouldn't be awesome if this worked on the active region instead of just the current line? Oh, baby!
(defun timvisher/drag-up ()
  (interactive)
  (let ((timvisher/position-in-line (timvisher/position-in-line)))
    (beginning-of-line)
    (kill-visual-line 1)
    (previous-line)
    (yank)
    (beginning-of-line)
    (previous-line)
    (goto-char (+ (point) timvisher/position-in-line))))

(defun timvisher/drag-down ()
  (interactive)
  (let ((timvisher/position-in-line (timvisher/position-in-line)))
    (beginning-of-line)
    (kill-visual-line)
    (next-line)
    (yank)
    (previous-line)
    (goto-char (+ (point) timvisher/position-in-line))))

(defun timvisher/position-in-line ()
  (save-excursion
    (let ((current-position (point)))
      (beginning-of-line)
      (- current-position (point)))))

(global-set-key (kbd "<M-s-down>") 'timvisher/drag-down)
(global-set-key (kbd "<M-s-up>") 'timvisher/drag-up)

;;; Let [Yegge][] speak the wisdom of the query-replace-regexp
(defalias 'qrr 'query-replace-regexp)

;;; ispell should be simple to type.
(defalias 's 'ispell)

(add-to-list 'auto-mode-alist '("\\.todo$" . org-mode))

(defun timvisher/turn-on-subword-mode ()
  (subword-mode 1))

;;; ----------------------------------------------------------------------------
;;; Alright, now let's start some stuff up that we always want started up
;;; ----------------------------------------------------------------------------

;;; We always want a server running for `emacsclient`, although the
;;; horror of operating outside of Emacs is not lost on us.
;;; 
;;; Note: If you get the dreaded `~/.emacs.d/server is not safe` error
;;; on Windows: Set `~/.emacs.d/server -> Properties -> Security ->
;;; Advanced -> Owner` to you.
(autoload 'server-running-p "server")
(unless (server-running-p) (server-start))

;;; Why shouldn't Emacs appear like it's your desktop? We all know it is
;; (if (display-graphic-p) (maximize-frame))
(when (display-graphic-p)
  (if (string-match "darwin" system-configuration) (ns-toggle-fullscreen) (maximize-frame)))

(global-set-key (kbd "C-c r SPC") 'cua-set-rectangle-mark)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(archive-extract-hook (quote (timvisher/make-read-only)))
 '(backup-directory-alist (\` (("." \, (concat (getenv "HOME") "/.emacs.d/" system-name "-backups")))))
 '(c-mode-common-hook (quote (timvisher/turn-on-subword-mode)))
 '(column-number-mode t)
 '(comment-column 0)
 '(css-indent-offset 3)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes (quote ("117284df029007a8012cae1f01c3156d54a0de4b9f2f381feab47809b8a1caef" default)))
 '(custom-theme-directory "~/.emacs.d/site-lisp/themes")
 '(deft-directory "")
 '(deft-extension "md")
 '(deft-text-mode (quote markdown-mode))
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(dired-use-ls-dired (quote unspecified))
 '(elein-lein "lein")
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#clojure" "#emacs"))))
 '(erc-autojoin-delay 30)
 '(erc-autojoin-mode t)
 '(erc-autojoin-timing (quote ident))
 '(erc-email-userid "tim.visher@gmail.com")
 '(erc-enable-logging (quote erc-log-all-but-server-buffers))
 '(erc-log-channels-directory "~/Dropbox/log")
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-nick-notify-cmd "notify")
 '(erc-nickserv-passwords (quote ((freenode (("timvisher" . "rideon"))))))
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-save-queries-on-quit t)
 '(erc-services-mode t)
 '(erc-text-matched-hook (quote (erc-log-matches)))
 '(erc-user-full-name "Tim Visher")
 '(find-ls-option (quote ("-print0 | xargs -0 ls -ld" . "-ld")))
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "target")))
 '(helm-M-x-always-save-history t)
 '(helm-c-use-adaptative-sorting t)
 '(helm-mode t)
 '(ido-everywhere t)
 '(ido-ubiquitous-command-exceptions (quote (unhighlight-regexp)))
 '(ido-ubiquitous-mode t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(inferior-lisp-program "lein repl")
 '(inhibit-startup-screen nil)
 '(js-indent-level 2)
 '(mouse-avoidance-mode (quote banish) nil (avoid))
 '(org-hide-leading-stars t)
 '(org-insert-heading-respect-content t)
 '(org-src-fontify-natively t)
 '(save-interprogram-paste-before-kill t)
 '(save-place-file (concat (getenv "HOME") "/.emacs.d/" system-name ".places"))
 '(sentence-end-double-space nil)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(solarized-contrast (quote high))
 '(sql-ms-options (quote ("-w" "15000" "-n")))
 '(sql-ms-program "osql")
 '(standard-indent 2)
 '(tab-width 2)
 '(text-mode-hook (quote (whitespace-mode text-mode-hook-identify)))
 '(transient-mark-mode nil)
 '(user-mail-address "tim.visher@gmail.com")
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(whitespace-style (quote (trailing space-before-tab face indentation space-after-tab)))
 '(winner-dont-bind-my-keys t)
 '(winner-mode t nil (winner)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-rectangle ((t (:inherit region))))
 '(esk-paren-face ((t (:foreground "grey55"))))
 '(magit-item-highlight ((t (:inherit hl-line))))
 '(match ((t (:inherit idle-highlight))))
 '(whitespace-indentation ((t (:inherit highlight :foreground "#e9e2cb"))))
 '(widget-field ((t (:inherit hl-line :box (:line-width 1 :color "#52676f"))))))

;;; We pay homage:
;;; [yegge]: https://sites.google.com/site/steveyegge2/effective-emacs
;;; [redisplay-dont-pause]: http://www.masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine/

;;; I like solarized. Only the cool kids use th3 dark mode, though.
(load-theme 'solarized-light)

(message "you're loaded, Charnock!")
