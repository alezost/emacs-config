;;; insert-pair.el --- Commands for inserting pairs of characters  -*- lexical-binding: t -*-

;; Copyright © 2015–2026 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 12 Jan 2015
;; Keywords: convenience

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

;; This package provides commands analogous to `insert-parentheses' but
;; for inserting other pairs of characters.
;;
;; These commands have the following names:
;; `insert-pair-curly-brackets', `insert-pair-double-quotations' and so
;; on (see `insert-pair-specifications' variable).  The commands may be
;; used in the same manner as the built-in `insert-parentheses' command.

;;; Code:

;;;###autoload
(defvar insert-pair-specifications
  '(("square-brackets"              ?\[ ?\])
    ("curly-brackets"               ?{ ?})
    ("angle-brackets"               ?< ?>)
    ("single-quotations"            ?' ?')
    ("double-quotations"            ?\" ?\")
    ("left-right-single-quotations" ?‘ ?’)
    ("left-right-double-quotations" ?“ ?”)
    ("angle-quotations"             ?« ?»)
    ("grave-accents"                ?` ?`)
    ("grave-accent-quotation"       ?` ?')
    ("top-corners"                  ?⌜ ?⌝)
    ("bottom-corners"               ?⌞ ?⌟))
  "List of pair specifications used to generate commands.")

(defmacro insert-pair-define-command (name open close)
  "Define command for inserting a pair of OPEN and CLOSE characters.
The command name is `insert-pair-NAME'."
  `(defun ,(intern (concat "insert-pair-" name)) (&optional arg)
     ,(format "Similar to `insert-parentheses', except it inserts %c%c."
              open close)
     (interactive "P")
     (insert-pair arg ,open ,close)))

(dolist (spec insert-pair-specifications)
  (eval `(insert-pair-define-command
          ,(nth 0 spec)
          ,(nth 1 spec)
          ,(nth 2 spec))))

;;;###autoload(dolist (spec insert-pair-specifications)
;;;###autoload  (autoload (intern (concat "insert-pair-" (car spec)))
;;;###autoload    "insert-pair" nil t))

(provide 'insert-pair)

;;; insert-pair.el ends here
