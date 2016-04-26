;;; al-font.el --- Additional functionality for working with fonts

;; Copyright © 2014-2016 Alex Kost

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

;;; Code:

(require 'cl-lib)


;;; Choosing the first available font

;; Idea from <http://www.emacswiki.org/emacs/SetFonts>.

(defvar al/font-candidates
  '("Liberation Mono-12" "DejaVu Sans Mono-11" "Terminus-12")
  "List of font names used by `al/first-existing-font'.")

(defun al/first-existing-font (&rest font-names)
  "Return first existing font from FONT-NAMES.
If FONT-NAMES is nil, use `al/font-candidates'."
  (cl-find-if (lambda (name)
                (find-font (font-spec :name name)))
              (or font-names al/font-candidates)))


;;; Setting different fonts for different characters

;; See (info "(emacs) Modifying Fontsets"),
;; <http://www.emacswiki.org/emacs/FontSets> and
;; <http://paste.lisp.org/display/133488> for more information.

(defun al/set-fontset (&optional name frame add specs)
  "Modify fontset NAME.
Each specification from SPECS list has the following form:

  (FONT . TARGETS)

TARGETS is a list of characters TARGET.  See `set-fontset-font'
for details."
  (dolist (spec specs)
    (pcase spec
      (`(,font . ,targets)
       (dolist (target targets)
         (set-fontset-font name target font frame add))))))

(provide 'al-font)

;;; al-font.el ends here
