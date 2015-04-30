;;; mdfind.el --- Use mdfind as your locate command  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Tim Visher

;; Author: Tim Visher <tim.visher@gmail.com>
;; Keywords: processes, tools, unix

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

;; Simply replaces locate with mdfind. For use with macs

;;; Code:

(setq locate-command "mdfind")

(defun mdfind-make-command-line (term)
  (list locate-command "-name" term))

(setq locate-make-command-line 'mdfind-make-command-line)

(provide 'mdfind)
;;; mdfind.el ends here
