;; Startup

(if (boundp 'mac-command-modifier) (setq mac-command-modifier 'meta))
(if (boundp 'mac-option-modifier) (setq mac-option-modifier 'super))

(require 'cl)
(setq whitespace-style '(trailing
                         space-before-tab
                         face
                         indentation
                         space-after-tab))
(labels
    ((add-path (p)
               (when p
                 (add-to-list 'load-path (concat "~/.emacs.d/elpa/" (car p)))
                 (add-path (cdr p)))))
  (add-path '("maxframe-0.5")))
(add-to-list 'load-path "~/.emacs.d/site-lisp")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/eclim")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/eclim/vendor")

;; (require 'eclim)

;; Functions

(defun lein-swank ()
  (interactive)
  (let ((root (locate-dominating-file default-directory "project.clj"))
        (current-directory default-directory))
    (when (not root)
      (error "Not in a Leiningen project."))
    ;; you can customize slime-port using .dir-locals.el
    (progn
      (cd root)
      (shell-command (format "lein swank &")
                     "*lein-swank*")
      (set-process-filter (get-buffer-process "*lein-swank*")
                          (lambda (process output)
                            (when (string-match "Connection opened on" output)
                              (slime-connect "localhost" slime-port)
                              (set-process-filter process nil))))
      (message "Starting swank server...")
      (cd current-directory))))

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

;; Yegge
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

;; Yegge
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; Yegge
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

;; http://www.ftrain.com/util_emacs_hints.html
(defun insert-time-stamp ()
  "Inserts a time stamp 'YYYY-MM-DD HH:MM AM/PM'"
  (interactive)
  (insert (format-time-string "%Y-%m-%d - %I:%M %p")))

(defun shell-bash ()
  "Create a new comint bash shell."
  (interactive)
  (let ((explicit-shell-file-name "c:/bin/bash"))
    (shell "*shell-bash*")))

(defun emacs-is-mac ()
  "Returns non-nil if Emacs is running on a Mac."
  (string-match "apple" (emacs-type)))

(defun emacs-is-windowed ()
  "Returns non-nil if Emacs is running in Windowed mode."
  (string-match "window" (emacs-type)))

;; Helpers
(defun emacs-type ()
  "My patched emacs-type.  Macintosh detection works correctly.

Original Documentation:
Returns the type of emacs in use.
Returns:
emacs - for regular Emacs in a console
emacs-window - for regular Emacs in a window(X)
xemacs - for XEmacs in a console
xemacs-window - for XEmacs in a window(X)
emacs-nt - for regular Emacs on Windows NT/9x in a console (NOT IMPLEMENTED YET)
emacs-nt-window - for regular Emacs on Windows NT/9x in a window(W32)
xemacs-nt - for XEmacs on Windows NT/9x in a console (NOT IMPLEMENTED YET)
xemacs-nt-window - for XEmacs on Windows NT/9x in a window(W32)
emacs-apple-window - for regular Emacs on Mac(??) in a window
emacs-msdos - for MS-DOS
unknown - Something unsupported"
  (cond
   ((string= "w32" window-system)
    "emacs-nt-window")
   ((string= "mswindows" window-system)
    "xemacs-nt-window")
   ((and (eq "msdos" system-type)
         (string-match "GNU" (emacs-version)))
    "emacs-msdos")
   ((or (and (string-match "apple" (emacs-version))
             (string-match "GNU" (emacs-version)))
        (boundp 'macintosh))
    "emacs-apple-window")
   ((or (eq system-type 'gnu/linux)
        (eq system-type 'linux))
    (if (string-match "XEmacs" emacs-version)
        (if window-system
            'xemacs-window
          "xemacs")
      (if window-system
          'emacs-window
        "emacs")))
   ((or (eq system-type 'windows)
        (string-match "GNU" (emacs-version)))
    "emacs-nt-window")
   (t "unknown")))

(defun width-from-display ()
  (- (/ (display-pixel-width) (frame-char-width)) 4))

(defun height-from-display ()
  (- (/ (display-pixel-height) (frame-char-height)) 5))

(defun markdown-frame ()
  "Create a buffer to quick edit markdown text."
  (interactive)
  (create-tmp-frame "markdown"))

(defun create-tmp-frame (prefix)
  "Create a frame PREFIXtimestamp."
  (let ((prefix-or-dimensions `(,prefix 85 ,(/ (height-from-display) 2))))
    (kill-matching-timestamped-buffers (prefix prefix-or-dimensions))
    (create-or-focus-frame prefix-or-dimensions)
    (set-frame-size (selected-frame) (width prefix-or-dimensions) (height prefix-or-dimensions))
    (find-timestamped-tmp-file (prefix prefix-or-dimensions))
    (yank)
    (beginning-of-buffer)))

(defun prefix (prefix-or-dimensions)
  (car prefix-or-dimensions))

(defun dimensions (prefix-or-dimensions)
  (cdr prefix-or-dimensions))

(defun width (prefix-or-dimensions)
  (cadr prefix-or-dimensions))

(defun height (prefix-or-dimensions)
  (caddr prefix-or-dimensions))

(defun create-or-focus-frame (prefix-and-dimensions)
  "Search for a frame titled PREFIX* or DIMENSIONS. If it exists,
   focus it. If it doesn't, create it and then focus it."
  (select-frame-set-input-focus (matching-frame-or-new-frame prefix-and-dimensions)))

(defun matching-frame-or-new-frame (prefix-and-dimensions)
  (make-frame))

(defun frame-exists (prefix-or-dimensions)
  (frame-parameter (next-frame) 'buffer-list))

(defun find-timestamped-tmp-file (prefix)
  "Visit timestamped-tmp-file prefixed with PREFIX."
  (find-file (timestamped-tmp-file prefix)))

(defun kill-matching-timestamped-buffers (prefix)
  "Kill timestamped-tmp-file buffers prefixed with PREFIX."
  (kill-matching-buffers (concat prefix "[0-9]+\.markdown")))

(defun timestamped-tmp-file (prefix)
  "Get a tmp file prefixed with PREFIX."
  (concat "~/tmp/" prefix (format-time-string "%Y%m%d%H%M%S") ".markdown"))

;; aliases

(defalias 'qrr 'query-replace-regexp) ;; Yegge
(defalias 's 'ispell)
(defalias 'mdf 'markdown-frame)

;; keys

(global-set-key (kbd "M-h") 'backward-kill-word)
;; (global-set-key (kbd "C-h") 'backward-delete-char)
;; (eval-after-load 'paredit '(define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word))

(global-set-key (kbd "<f1> r") 'info-emacs-manual)

(put 'set-goal-column 'disabled nil)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'glasses-mode)
(add-hook 'text-mode-hook 'whitespace-mode)
;; (add-hook 'groovy-mode-hook 'run-prog-mode-hook)
(remove-hook 'text-mode-hook 'turn-on-flyspell)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'visual-line-mode)

(setq w32-lwindow-modifier 'super)
(setq w32-rwindow-modifier 'super)
(setq w32-pass-lwindow-to-system nil)
(setq w32-pass-rwindow-to-system nil)
(setq w32-apps-modifier 'hyper)

(set-register ?t '"//mlvv258a/temp")
(set-register ?s "//mlvv258a/was/v61/AppServer/profiles/SF_0501_TEST/installedApps/TEST_00/SF_0303_TEST_TNT33_0000.ear")
(set-register ?c '(file . "d:/view_store/z002w5en_view"))

(setq transient-mark-mode nil)

(require 'maxframe)
(maximize-frame)

(shell)

;; (server-start)
