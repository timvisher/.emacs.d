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

(require 'cl)

(labels
    ((add-path (p)
               (when p
                 (add-to-list 'load-path (concat "~/.emacs.d/elpa/" (car p)))
                 (add-path (cdr p)))))
  (add-path '("maxframe-0.5")))

(add-to-list 'load-path "~/.emacs.d/site-lisp")

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

(defun drag-up ()
  (interactive)
  (progn (kill-visual-line 1)
         (previous-line)
         (yank)
         (previous-line)))

(defun drag-down ()
  (interactive)
  (progn (kill-visual-line 1)
         (next-line)
         (yank)
         (previous-line)))

(global-set-key (kbd "<M-S-down>") 'drag-down)
(global-set-key (kbd "<M-S-up>") 'drag-up)

(defun lein-swank ()
  (interactive)
  (let ((root (locate-dominating-file default-directory "project.clj")))
    (when (not root)
      (error "Not in a Leiningen project."))
    ;; you can customize slime-port using .dir-locals.el
    (shell-command (format "cd %s && lein swank &" root)
                   "*lein-swank*")
    (set-process-filter (get-buffer-process "*lein-swank*")
                        (lambda (process output)
                          (when (string-match "Connection opened on" output)
                            (slime-connect "localhost" slime-port)
                            (set-process-filter process nil))))
    (message "Starting swank server...")))

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

(defun markdown-frame ()
  "Create a buffer to quick edit markdown text."
  (interactive)
  (create-tmp-frame "markdown"))

(defun create-tmp-frame (prefix)
  "Create a frame PREFIXtimestamp."
  ;; (let ((prefix-or-dimensions `(,prefix 85 ,(/ (height-from-display) 2))))
  ;;   (kill-matching-timestamped-buffers prefix)
  ;;   (create-or-focus-frame prefix)
  ;;   ;; (set-frame-size (selected-frame) (width prefix-or-dimensions) (height prefix-or-dimensions))
  ;;   (find-timestamped-tmp-file prefix)
  ;;   (yank)
  ;;   (beginning-of-buffer))
    (kill-matching-timestamped-buffers prefix)
    (create-or-focus-frame prefix)
    ;; (set-frame-size (selected-frame) (width prefix-or-dimensions) (height prefix-or-dimensions))
    (find-timestamped-tmp-file prefix)
    (yank)
    (beginning-of-buffer)  )

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

(defun kmacro-edit-lossage ()
  "Edit most recent 300 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro 'view-lossage))

(global-set-key (kbd "M-h") 'backward-kill-word)
;; (global-set-key (kbd "C-h") 'backward-delete-char-untabify)
;; (eval-after-load 'paredit '(define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word))
(eval-after-load 'markdown-mode '(define-key markdown-mode-map (kbd "M-v") 'yank))

(global-set-key (kbd "<f1> r") 'info-emacs-manual)

(put 'set-goal-column 'disabled nil)

(setq whitespace-style '(trailing
                         space-before-tab
                         face
                         indentation
                         space-after-tab))

;;; Make re-builder copy just the text without the `"` characters.
(defun reb-copy ()
  "Copy current RE into the kill ring for later insertion."
  (interactive)

  (reb-update-regexp)
  (let ((re (with-output-to-string
	      (print (reb-target-binding reb-regexp)))))
    (kill-new (substring re 2 (- (length re) 2)))
    (message "Regexp copied to kill-ring")))

(add-hook 'coding-hook 'hs-minor-mode)
(add-hook 'coding-hook (lambda () (textmate-mode 1)))
(add-hook 'coding-hook 'glasses-mode)
(add-hook 'coding-hook 'whitespace-mode)
(add-hook 'groovy-mode-hook 'run-coding-hook)
(add-hook 'applescript-mode-hook 'run-coding-hook)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-flyspell)
(remove-hook 'coding-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'whitespace-mode)

(setq transient-mark-mode nil)

;; (load-theme 'solarized-dark)
;; (enable-theme 'solarized-dark)

(setq ido-ubiquitous-enabled nil)

(require 'maxframe)
(maximize-frame)

(shell)

(put 'narrow-to-region 'disabled nil)

(fset 'vimgolf-harvest
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217788 25 67108896 134217790 23 134217788 67108896 5 134217847 1 19 115 116 97 114 116 32 102 105 108 101 1 16 134217848 97 112 45 116 45 102 13 118 105 109 9 21 24 113 32 25 46 112 114 101 115 101 110 116 97 116 105 111 110 46 109 100 13 14 14 14 19 101 110 100 32 102 105 108 101 1 134217848 97 112 45 116 45 102 13 118 105 109 9 21 24 113 9 134217832 134217832 115 116 97 114 116 46 21 24 113 13 134217848 97 112 45 116 45 102 13 118 105 109 9 21 24 113 9 119 111 114 107 46 21 24 113 13 14 14 67108896 134217790 134217848 97 112 45 116 45 102 13 118 105 109 9 21 24 113 9 101 110 100 46 21 24 113 13 134217788 82 101 115 101 97 114 99 104 32 86 105 109 71 111 108 102 32 105 110 32 69 109 97 99 115 32 48 21 24 113 32 5 32 35 64 119 101 98 32 64 104 111 109 101 32 104 116 116 112 58 47 47 118 105 109 103 111 108 102 134217826 106 46 109 112 47 5 48 21 24 113 1 11 11 25 25 25 16 16 134217828 82 101 99 111 114 100 14 1 134217828 80 117 98 108 105 115 104 134217788 67108896 14 14 14 134217848 97 112 45 116 45 98 13 118 105 109 103 9 13] 0 "%d")) arg)))

(server-start)
