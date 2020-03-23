;;; al-text.el --- Additional functionality related to text editing

;; Copyright © 2014–2016, 2020 Alex Kost

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

(defun al/beginning-of-buffer ()
  (goto-char (point-min)))

(defun al/show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(defun al/no-truncate-lines ()
  (setq-local truncate-lines nil))

(defun al/inhibit-field-motion ()
  (setq-local inhibit-field-text-motion t))

(defun al/bar-cursor-type ()
  (setq-local cursor-type 'bar))

(defun al/hbar-cursor-type ()
  (setq-local cursor-type 'hbar))

(defun al/no-syntactic-font-lock ()
  (setq-local font-lock-keywords-only t))

(defun al/set-comment-column ()
  (setq-local comment-column 32))

(defmacro al/modify-syntax (table-name &rest specs)
  "Update syntax table according to SPECS.
TABLE-NAME is a name (unquoted symbol) of a syntax table variable.
SPECS are (CHAR NEWENTRY) elements.  See `modify-syntax-entry'
for details."
  (declare (indent 1))
  (let ((table-var (make-symbol "table")))
    `(al/with-check
       :var ',table-name
       (let ((,table-var (symbol-value ',table-name)))
         ,@(mapcar
            (lambda (spec)
              (pcase spec
                (`(,char ,entry)
                 `(modify-syntax-entry ,char ,entry ,table-var))))
            specs)))))

(defmacro al/modify-page-break-syntax (table-name)
  "Set non-whitespace syntax for ^L in syntax table TABLE-NAME.
Page break should not belong to whitespace syntax, because
`back-to-indentation' moves the point after ^L character which is not good.
Also it (default syntax) breaks `indent-guide-mode'."
  `(al/modify-syntax ,table-name (?\f ">   ")))

(defun al/replace (from-string to-string &optional start end)
  "This is similar to `query-replace' but without querying.

Replace some occurrences of FROM-STRING with TO-STRING in the
current buffer.

START and END specify the region to operate on.  If they are not
specified, then the currenty marked region is used.  If there is
no marked region, then the whole buffer is used."
  (let ((count 0)
        beg end)
    (if (use-region-p)
        (setq beg (region-beginning)
              end (region-end)))
    (save-excursion
      (when beg (goto-char beg))
      (while (re-search-forward (regexp-quote from-string) end t)
        (replace-match to-string)
        (setq count (1+ count))))
    (unless (= 0 count)
      (message "'%s' has been replaced with '%s' %d time(s)."
               from-string to-string count))))

(provide 'al-text)

;;; al-text.el ends here
