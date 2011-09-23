(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("0174d99a8f1fdc506fa54403317072982656f127" default)))
 '(custom-theme-directory "~/.emacs.d/site-lisp/themes")
 '(erc-autojoin-channels-alist (quote (("irc.freenode.net" "#emacs" "#clojure"))))
 '(erc-autojoin-delay 30)
 '(erc-autojoin-mode t)
 '(erc-email-userid "tim.visher@gmail.com")
 '(erc-nick "timvisher")
 '(erc-user-full-name "Tim Visher"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
