(if (boundp 'mac-command-modifier) (setq mac-command-modifier 'meta))
(if (boundp 'mac-option-modifier) (setq mac-option-modifier 'super))

;;; ----------------------------------------------------------------------------
;;; Start up default processes
;;; ----------------------------------------------------------------------------

;;; Always run ERC on my mac.
(erc :server "irc.freenode.net" :nick "ttimvisher")

