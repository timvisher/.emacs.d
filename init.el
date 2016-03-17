;;; ----------------------------------------------------------------------------
;;; We wants our load-path set up properly
;;; ----------------------------------------------------------------------------

;;; We need site-lisp to be here for stuff that hasn't made it out of the wilderness and into elpa
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;; All of our elisp hackery goes under here.
(add-to-list 'load-path "~/.emacs.d/timvisher")

;;; ELPA FTW!
(load "timvisher_package")

;;; ----------------------------------------------------------------------------
;;; And now let's get some system wide variables set up
;;; ----------------------------------------------------------------------------

(load "timvisher_keys")

(defalias 'yes-or-no-p 'y-or-n-p)

;;; You always wants your custom info files to be in the Info-directory-list
(eval-after-load 'info 
  '(add-to-list 'Info-directory-list (concat (getenv "HOME") "/.emacs.d/info")))

Info-directory-list

;;; ----------------------------------------------------------------------------
;;; Alright, time's up. Let's do enabling and configuring modes.
;;; ----------------------------------------------------------------------------

;; (electric-pair-mode 1)
;; (electric-indent-mode 1)

(ido-ubiquitous-mode 1)

(flx-ido-mode 1)

(ido-vertical-mode 1)

(projectile-global-mode 1)

(setq projectile-enable-caching t)

(setq projectile-mode-line " Projectile")

;;; Supposedly I need to turn this on to get scroll wheel support (like gnome-terminal) in iTerm but it also captures the mouse for things like cursor selection which breaks iterm selection clipboard copying. Not sure if I like this.
;; (xterm-mouse-mode 1)
;; (global-set-key (kbd "<mouse-4>") 'next-line)
;; (global-set-key (kbd "<mouse-5>") 'previous-line)

;; ;;; If we really, really don't want ido-mode, this is how to do it!
;; (eval-after-load 'starter-kit-misc
;;   '(ido-mode nil))
;; (eval-after-load 'starter-kit-misc
;;   '(ido-ubiquitous nil))

(global-set-key (kbd "C-c k") 'kill-whole-line)

;; (defalias 'csr 'cua-set-rectangle-mark)

(load "timvisher_flush-lines")

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

(require 'bookmarklets)

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

(load "timvisher_python-customizations")

(load "timvisher_paredit")

(load "timvisher_clojure")

(load "timvisher_css")

(load "timvisher_git")

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)

(when (featurep 'google-this)
    (google-this-mode 1))

(defun timvisher/load-yas-snippets ()
  (yas/load-directory "~/.emacs.d/snippets"))

;;; Looky, looky, I've got hooky
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)
(if (locate-library "yasnippet")
    (lambda ()
      (autoload 'yas-minor-mode-on "yasnippet")
      (add-hook 'prog-mode-hook 'yas-minor-mode-on)
      (eval-after-load 'yasnippet
        '(add-hook 'yas-minor-mode-hook 'timvisher/load-yas-snippets))))
(remove-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(add-hook 'applescript-mode-hook 'run-prog-mode-hook)

(remove-hook 'text-mode-hook 'turn-on-flyspell)

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

(defun timvisher/edit-init-file ()
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c i") 'timvisher/edit-init-file)

(defun timvisher/host-init-file ()
  (format "~/.emacs.d/%s.el" system-name))

(defun timvisher/edit-host-init-file ()
  (interactive)
  (find-file (timvisher/host-init-file)))

(when (executable-find "pbcopy")
  (defun timvisher/pbpaste ()
    (interactive)
    (let ((default-directory "~"))
      (with-temp-buffer
        (shell-command "pbpaste" 't)
        (kill-region (point-min) (point-max)))
      (yank)))

  (global-set-key (kbd "C-c P y") 'timvisher/pbpaste)

  (defun timvisher/pbcopy-region ()
    (let ((default-directory "~"))
      (shell-command-on-region (region-beginning) (region-end) "pbcopy")))

  (defun timvisher/pbcopy ()
    (interactive)
    (timvisher/pbcopy-region)
    (kill-ring-save (region-beginning) (region-end)))

  (global-set-key (kbd "C-c P w") 'timvisher/pbcopy)

  (defun timvisher/pbcopy-string (string)
    (with-temp-buffer
      (insert string)
      (shell-command-on-region (point-min) (point-max) "pbcopy")
      (kill-new string)))

  (defun timvisher/pbkill ()
    (interactive)
    (timvisher/pbcopy-region)
    (kill-region (region-beginning) (region-end)))

  (global-set-key (kbd "C-c P k") 'timvisher/pbkill))

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

(add-to-list 'auto-mode-alist '("\\.tpl$" . handlebars-mode))

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

;; (global-set-key (kbd "C-c r SPC") 'cua-set-rectangle-mark)

;;; Custom's in it's own file because having it constantly editing your init.el file sucks
(load "custom")

(load-sensitive-files)

;;; Load system specific configuration

(when (file-exists-p (timvisher/host-init-file))
  (load-file (timvisher/host-init-file)))

;;; We pay homage:
;;; [yegge]: https://sites.google.com/site/steveyegge2/effective-emacs
;;; [redisplay-dont-pause]: http://www.masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine/
