;;; al-lisp.el --- Additional functionality for `lisp-mode'

;; Copyright © 2017 Alex Kost

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


;;; Highlighting "defcommand" (used by StumpWM)

;; To highlight docstring properly (with `font-lock-doc-face').
(put 'defcommand 'doc-string-elt 4)

(defvar al/lisp-defcommand-regexp
  (rx "(" (group "defcommand")
      symbol-end
      (zero-or-more blank)
      (zero-or-one "(")
      (zero-or-one
       (group (one-or-more (or (syntax word) (syntax symbol))))))
  "Regexp to match 'defcommand' keyword.")

(defun al/lisp-add-defcommand-font-lock-keywords ()
  "Add font-lock keywords to highlight 'defcommand' properly.
Call this function once!"
  (font-lock-add-keywords
   'lisp-mode
   `((,al/lisp-defcommand-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t)))))

(provide 'al-lisp)

;;; al-lisp.el ends here
