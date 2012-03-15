(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(require 'info)
(add-to-list 'Info-directory-list (concat (getenv "HOME") "/.emacs.d/info"))

(setq send-mail-function 'mailclient-send-it)
(setq message-send-mail-function 'message-send-mail-with-mailclient)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
                      clojure-test-mode
                      deft
                      elein
                      elisp-slime-nav
                      furl
                      idle-highlight-mode
                      ido-ubiquitous
                      magit
                      markdown-mode
                      marmalade
                      maxframe
                      slime
                      smex
                      find-file-in-project
                      paredit
                      starter-kit
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-lisp
                      textmate
                      vimgolf))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(eval-after-load "deft" '(defun journal ()
                           "Grab a new deft file and populate it with a joural entry for right now"
                           (interactive)
                           (select-frame-set-input-focus (make-frame))
                           (deft-new-file)
                           (visual-line-mode 1)
                           (insert "journal entry " (format-time-string "%Y%m%d%H%M%S") "

")))

(defun copy-buffer-and-kill-frame ()
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (delete-frame))

(eval-after-load "deft" '(defun kill-ring-deft ()
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
                           (local-set-key (kbd "C-c C-q") 'copy-buffer-and-kill-frame)))

(defun lein-server ()
  (interactive)
  (let ((current-directory default-directory)
        (lein-home (locate-dominating-file default-directory "project.clj")))
    (progn
      (cd lein-home)
      (async-shell-command "lein ring server" "*lein server*")
      (cd current-directory))))

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

;; aliases

(defalias 'qrr 'query-replace-regexp) ;; Yegge
(defalias 's 'ispell)
(defalias 'mdf 'kill-ring-deft)

;; keys

(defun kmacro-edit-lossage ()
  "Edit most recent 300 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro 'view-lossage))

(defun fix-paredit-repl ()
  (interactive)
  (local-set-key "{" 'paredit-open-curly)
  (local-set-key "}" 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")["))

(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.todo$" . org-mode))
(eval-after-load 'slime '(fix-paredit-repl))

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

(defun turn-on-textmate-mode ()
  (textmate-mode 1))

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'turn-on-textmate-mode)
(add-hook 'prog-mode-hook 'glasses-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'applescript-mode-hook 'run-prog-mode-hook)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-flyspell)
(remove-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'whitespace-mode)

(require 'elein)

(fset 'vimgolf-harvest
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217788 25 67108896 134217790 23 134217788 67108896 5 134217847 1 19 115 116 97 114 116 32 102 105 108 101 1 16 134217848 97 112 45 116 45 102 13 118 105 109 9 21 24 113 32 25 46 112 114 101 115 101 110 116 97 116 105 111 110 46 109 100 13 14 14 14 19 101 110 100 32 102 105 108 101 1 134217848 97 112 45 116 45 102 13 118 105 109 9 21 24 113 9 134217832 134217832 115 116 97 114 116 46 21 24 113 13 134217848 97 112 45 116 45 102 13 118 105 109 9 21 24 113 9 119 111 114 107 46 21 24 113 13 14 14 67108896 134217790 134217848 97 112 45 116 45 102 13 118 105 109 9 21 24 113 9 101 110 100 46 21 24 113 13 134217788 82 101 115 101 97 114 99 104 32 86 105 109 71 111 108 102 32 105 110 32 69 109 97 99 115 32 48 21 24 113 32 5 32 35 64 119 101 98 32 64 104 111 109 101 32 104 116 116 112 58 47 47 118 105 109 103 111 108 102 134217826 106 46 109 112 47 5 48 21 24 113 1 11 11 25 25 25 16 16 134217828 82 101 99 111 114 100 14 1 134217828 80 117 98 108 105 115 104 134217788 67108896 14 14 14 134217848 97 112 45 116 45 98 13 118 105 109 103 9 13] 0 "%d")) arg)))

;;; If you get the dreaded ~/.emacs.d/server is not safe error on
;;; Windows. ~/.emacs.d/server -> Properties -> Security -> Advanced
;;; -> Owner and then set it to you.
(server-start)

(maximize-frame)

;;; Customizations

;;; Make system and user specific emacs temp files
(setq eshell-history-file-name (concat (getenv "HOME") "/.emacs.d/eshell/" system-name "-history"))

(setq redisplay-dont-pause t)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(backup-directory-alist (\` (("." \, (concat (getenv "HOME") "/.emacs.d/" system-name "-backups")))))
 '(css-indent-offset 3)
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes (quote ("62b81fe9b7d13eef0539d6a0f5c0c37170c9e248" "5600dc0bb4a2b72a613175da54edb4ad770105aa" "0174d99a8f1fdc506fa54403317072982656f127" default)))
 '(custom-theme-directory "~/.emacs.d/site-lisp/themes")
 '(deft-directory "")
 '(deft-extension "md")
 '(deft-text-mode (quote markdown-mode))
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(dired-use-ls-dired (quote unspecified))
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#clojure"))))
 '(erc-autojoin-delay 30)
 '(erc-autojoin-mode t)
 '(erc-email-userid "tim.visher@gmail.com")
 '(erc-nick "timvisher")
 '(erc-nick-notify-cmd "notify")
 '(erc-prompt-for-password t)
 '(erc-user-full-name "Tim Visher")
 '(find-ls-option (quote ("-print0 | xargs -0 ls -ld" . "-ld")))
 '(global-hl-line-mode nil)
 '(grep-find-template "find . <X> -type f <F> -exec grep <C> -nH <R> {} ;")
 '(ido-ubiquitous-command-exceptions (quote (unhighlight-regexp)))
 '(ido-ubiquitous-mode t)
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "lein repl")
 '(inhibit-startup-screen nil)
 '(js-indent-level 2)
 '(marmalade-server "http://marmalade-repo.org")
 '(marmalade-token "3RChoHxcXN1NRWDVJGPQ0JA6dR8+E8VvQZOuwWWQgtQ=")
 '(marmalade-username "timvisher")
 '(mouse-avoidance-mode (quote banish) nil (avoid))
 '(save-place-file (concat (getenv "HOME") "/.emacs.d/" system-name ".places"))
 '(sentence-end-double-space nil)
 '(tab-width 2)
 '(text-mode-hook (quote (whitespace-mode text-mode-hook-identify)))
 '(transient-mark-mode nil)
 '(user-mail-address "tim.visher@gmail.com")
 '(vimgolf-key "da83e81eff86621ca95f8af89c387d19"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t (:inherit hl-line))))
 '(widget-field ((t (:inherit hl-line :box (:line-width 1 :color "#52676f"))))))
(put 'upcase-region 'disabled nil)
