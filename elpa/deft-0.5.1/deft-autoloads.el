;;; deft-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "deft" "deft.el" (21689 12282 494830 421000))
;;; Generated autoloads from deft.el

(autoload 'deft-new-file "deft" "\
Create a new file quickly.
Use either an automatically generated filename or the filter
string if non-nil and `deft-use-filename-as-title' is set.  If the
filter string is non-nil and title is not from filename, use it
as the title.

\(fn)" t nil)

(autoload 'deft "deft" "\
Switch to *Deft* buffer and load files.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; deft-autoloads.el ends here
