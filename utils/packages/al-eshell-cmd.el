;;; al-eshell-cmd.el --- Additional commands for `eshell' package  -*- lexical-binding: t -*-

;; Copyright © 2014–2026 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains commands that start `eshell'.  They are separated
;; from "al-eshell.el" to avoid recursive loading.  For example, suppose
;; `al/eshell' is placed in "al-eshell.el".  Then, when autoloaded
;; `al/eshell' command is called, Emacs loads "al-eshell.el" which
;; requires eshell modules, which load my settings for eshell, which
;; require `al-eshell' feature which does not exist yet, so
;; "al-eshell.el" is loaded again.

;;; Code:

(require 'al-buffer)

(declare-function eshell/cd "em-dirs")

(defun al/eshell-buffers (&optional no-sort)
  "Return a list of all eshell buffers.
If NO-SORT is non-nil, do not sort the list by buffer names."
  (al/buffers-by-mode 'eshell-mode
                      (unless no-sort #'al/buffer-name<)))

;;;###autoload
(defun al/eshell (&optional arg)
  "Start eshell if needed or switch to the next \\[eshell] buffer.
If ARG is non-nil, start a new eshell buffer."
  (interactive "P")
  (if arg
      (eshell 'new)
    (al/rotate-or-select-buffer (al/eshell-buffers) #'eshell)))

;;;###autoload
(defun al/eshell-cd (arg)
  "Start eshell and change directory there to the current one.
ARG has the same meaning as in `eshell'"
  (interactive "P")
  (let ((dir default-directory))
    (eshell arg)
    (require 'em-dirs)
    (eshell/cd dir)))

(provide 'al-eshell-cmd)

;;; al-eshell-cmd.el ends here
