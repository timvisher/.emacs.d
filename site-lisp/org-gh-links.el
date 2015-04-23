;;; org-gh-links.el --- Adds support for link to github issues to org mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Tim Visher

;; Author: Tim Visher <tim.visher@gmail.com>
;; Keywords: convenience, extensions, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Allows linking to github issues like `gh-i:org/repo#1`.
;; Other things to link to? `gh:org[/repo]`, `gh-pr:org/repo#1`?

;;; Code:

(require 'org)

(defun org-gh-links-issue-open (path)
  "Visit the GitHub link for PATH"
  (let* ((matches      (s-match "\\(.+\\)/\\(.+\\)#\\([[:digit:]]+\\)" path))
         (organization (cadr matches))
         (repo         (caddr matches))
         (issue-number (cadddr matches))
         (issue-url    (format "https://github.com/%s/%s/issues/%s"
                               organization
                               repo
                               issue-number)))
    (browse-url issue-url)))

(defun org-gh-links-issue-store-link ()
  "Store a link to a GitHub issue"
  ;; TODO This might be useful if we could also browse github issues
  ;; in emacs.
  )

(org-add-link-type "gh-i" 'org-gh-links-issue-open)
;; (add-hook 'org-store-link-functions 'org-gh-links-issue-store-link) ; TODO this is how we'd add it in


(provide 'org-gh-links)
;;; org-gh-links.el ends here
