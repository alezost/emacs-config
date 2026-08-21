;;; al-magit.el --- Additional functionality for `magit' package  -*- lexical-binding: t -*-

;; Copyright © 2016–2026 Alex Kost

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

;;; Code:

(require 'magit)
(require 'git-commit)
(require 'al-buffer)

(defun al/magit-buffers (&optional type no-sort)
  "Return a list of TYPE magit buffers.

TYPE can be one of the following symbols:
  `all' (default) - all magit buffers,
  `status' - only status buffers.

If NO-SORT is non-nil, do not sort the list by buffer names."
  (let ((mode (if (eq type 'status)
                  'magit-status-mode
                'magit-mode)))
    (al/buffers-by-mode mode
                        (unless no-sort #'al/buffer-name<))))

;; Although this keymap is used by `al/magit-switch-buffer' from
;; `al-magit-cmd', it is placed here because it is set by my config
;; which requires `al-magit' but not `al-magit-cmd'.
(defvar al/magit-switch-map (make-sparse-keymap))

(defun al/git-commit-co-authored (name mail)
  "Insert a header acknowledging that you have co-authored the commit."
  (interactive (git-commit-self-ident))
  (git-commit--insert-ident-trailer "Co-authored-by" name mail))

(provide 'al-magit)

;;; al-magit.el ends here
