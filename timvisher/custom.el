(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(archive-extract-hook (quote (timvisher/make-read-only)))
 '(auto-insert (quote not-modified))
 '(auto-insert-mode t)
 '(background-color "#ffffff")
 '(background-mode dark)
 '(backup-directory-alist (\` (("." \, (concat (getenv "HOME") "/.emacs-backups")))))
 '(c-mode-common-hook (quote (timvisher/turn-on-subword-mode)))
 '(cider-repl-use-pretty-printing t)
 '(column-number-mode t)
 '(comment-column 0)
 '(compilation-message-face (quote default))
 '(css-indent-offset 3)
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cua-remap-control-v nil)
 '(cua-remap-control-z nil)
 '(cua-toggle-set-mark nil)
 '(cursor-color "#ffff00")
 '(custom-enabled-themes nil)
 '(custom-file "~/.emacs.d/timvisher/custom.el")
 '(custom-safe-themes
   (quote
    ("f22a0f5b85aed98055e4e5013cc104829d163067c03f8165ab03ae010d6e3d40" "ff9e6deb9cfc908381c1267f407b8830bcad6028231a5f736246b9fc65e92b44" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "5debeb813b180bd1c3756306cd8c83ac60fda55f85fb27249a0f2d55817e3cab" "117284df029007a8012cae1f01c3156d54a0de4b9f2f381feab47809b8a1caef" default)))
 '(custom-theme-directory "~/.emacs.d/site-lisp/themes")
 '(deft-directory "")
 '(deft-extension "md")
 '(deft-text-mode (quote markdown-mode))
 '(delete-selection-mode t)
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(dired-use-ls-dired (quote unspecified))
 '(erc-autoaway-mode t)
 '(erc-autojoin-channels-alist
   (quote
    (("freenode.net" "#clojure" "#emacs" "#nethack4" "#NetHack" "#ubuntu"))))
 '(erc-autojoin-delay 30)
 '(erc-autojoin-mode t)
 '(erc-autojoin-timing (quote ident))
 '(erc-email-userid "tim.visher@gmail.com")
 '(erc-enable-logging (quote erc-log-all-but-server-buffers))
 '(erc-keywords (quote ("\"vimgolf\"" "\"async\"")))
 '(erc-log-channels-directory "~/Dropbox/log")
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-nick "timvisher" t)
 '(erc-nick-notify-cmd "notify")
 '(erc-nick-uniquifier "t")
 '(erc-notifications-mode t)
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-save-queries-on-quit t)
 '(erc-services-mode t)
 '(erc-text-matched-hook (quote (erc-log-matches)))
 '(erc-user-full-name "Tim Visher")
 '(fci-rule-color "#eee8d5")
 '(find-ls-option (quote ("-print0 | xargs -0 ls -ld" . "-ld")))
 '(foreground-color "#ffff00")
 '(gc-cons-threshold 52428800)
 '(git-commit-max-summary-line-length 80)
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(global-whitespace-mode t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "target")))
 '(helm-M-x-always-save-history t)
 '(helm-c-use-adaptative-sorting t)
 '(helm-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(history-length 1000)
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(ido-everywhere t)
 '(ido-save-directory-list-file "~/.emacs-ido.last")
 '(ido-ubiquitous-command-exceptions (quote (unhighlight-regexp)))
 '(ido-ubiquitous-mode t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(inferior-lisp-program "lein repl")
 '(inhibit-startup-screen nil)
 '(isearch-allow-scroll t)
 '(ispell-program-name "aspell")
 '(js-indent-level 2)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(markdown-css-paths (quote ("css/bootstrap.css" "css/custom.css")))
 '(mouse-avoidance-mode (quote banish) nil (avoid))
 '(org-agenda-files
   (quote
    ("~/Dropbox/what-does-test-check-do-qmark/what-does-test-check-do-qmark.org" "~/Dropbox/lunchandlearn.org" "~/Dropbox/wiki/wiki.org" "~/Dropbox/rjmetrics-wiki.org" "~/Dropbox/budget-review.org")))
 '(org-hide-leading-stars t)
 '(org-insert-heading-respect-content t)
 '(org-src-fontify-natively t)
 '(package-selected-packages
   (quote
    (yaml-mode anti-zenburn-theme zenburn-theme solarized-theme yasnippet wgrep web-mode vimgolf smex smartparens sensitive sass-mode rainbow-delimiters projectile php-mode pbcopy paredit marmalade markdown-mode magit ido-vertical-mode ido-ubiquitous idle-highlight-mode golden-ratio flx-ido find-file-in-project expand-region elisp-slime-nav deft dash-functional coffee-mode clojure-test-mode better-defaults align-cljlet ag)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(projectile-cache-file "~/.emacs-projectile.cache")
 '(projectile-known-projects-file "~/.emacs.-projectile-bookmarks.eld")
 '(save-interprogram-paste-before-kill t)
 '(save-place-file (concat (getenv "HOME") "/.emacs.places"))
 '(savehist-file "~/.emacs-history")
 '(savehist-mode t)
 '(sensitive-root "~/Dropbox/sensitive")
 '(sentence-end-double-space nil)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(shell-prompt-pattern
   "^[^#$%>
]*[#$%>🐀 🐁 🐂 🐃 🐄 🐅 🐆 🐇 🐈 🐉 🐊 🐋 🐌 🐍 🐎 🐏 🐐 🐑 🐒 🐓 🐔 🐕 🐖 🐗 🐘 🐙 🐚 🐛 🐜 🐝 🐞 🐟 🐠 🐡 🐢 🐣 🐤 🐥 🐦 🐧 🐨 🐩 🐪 🐫 🐬🐭 🐮 🐯 🐰 🐱 🐲 🐳 🐴 🐵 🐶 🐷 🐸 🐹 🐺 🐻 🐼 🐽 🐾 😸 😹 😺 😻 😼 😽 😾 😿 🙀 🙈 🙉 🙊] *")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(solarized-contrast (quote high))
 '(split-width-threshold 240)
 '(sql-ms-options (quote ("-w" "15000" "-n")))
 '(sql-ms-program "osql")
 '(standard-indent 2)
 '(tab-width 2)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(text-mode-hook (quote (whitespace-mode text-mode-hook-identify)))
 '(transient-mark-mode nil)
 '(user-mail-address "tim.visher@gmail.com")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c37300")
     (60 . "#b97d00")
     (80 . "#b58900")
     (100 . "#a18700")
     (120 . "#9b8700")
     (140 . "#948700")
     (160 . "#8d8700")
     (180 . "#859900")
     (200 . "#5a942c")
     (220 . "#439b43")
     (240 . "#2da159")
     (260 . "#16a870")
     (280 . "#2aa198")
     (300 . "#009fa7")
     (320 . "#0097b7")
     (340 . "#008fc7")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(whitespace-line-column 120)
 '(whitespace-style
   (quote
    (trailing space-before-tab face indentation space-after-tab tabs lines face tab-mark)))
 '(winner-dont-bind-my-keys t)
 '(winner-mode t nil (winner)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:inherit hl-line :foreground "#e5e5e5")))))
