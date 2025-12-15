;;; al-tex.el --- Additional functionality related to (La)TeX modes  -*- lexical-binding: t -*-

;; Copyright © 2022–2025 Alex Kost

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


;;; LaTeX ↔ Unicode conversion

(require 'tex-mode) ; for `tex--prettify-symbols-alist'

(defun al/convert-symbol-to-latex (symbol)
  "Convert Unicode SYMBOL to LaTeX if possible.
Return a string containing the according LaTeX macro or nil
if SYMBOL (which can be a character or a one-character string)
was not converted."
  (if-let* ((char (cond ((characterp symbol) symbol)
                        ((stringp symbol) (aref symbol 0)))))
      ;; Ignore usual letters, digits, newlines, spaces, etc.
      (unless (string-match-p "[a-zA-Z0-9[:cntrl:][:blank:]]"
                              (string char))
        (car (rassoc char tex--prettify-symbols-alist)))
    (error "SYMBOL must be a character or a string")))

(defun al/convert-string-to-latex (string)
  "Convert STRING with Unicode symbols to LaTeX."
  (with-temp-buffer
    (mapc (lambda (char)
            (let ((latex (al/convert-symbol-to-latex char)))
              (insert (if latex
                          ;; We insert extra spaces to avoid
                          ;; "{1}" → "\\lbrace1\\rbrace" conversion.
                          (concat " " latex " ")
                        char))))
          string)

    ;; Removing extra spaces.
    (goto-char (point-min))
    (while (re-search-forward "  +" nil t) ; 2 spaces or more
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "^ \\| $" nil t) ; spaces at beg/end of lines
      (replace-match ""))
    (goto-char (point-min))
    ;; Spaces after/before opening/closing braces.
    (while (re-search-forward
            (rx (group (or "(" "[" "\\{")) " ")
            nil t)
      (replace-match "\\1"))
    (goto-char (point-min))
    (while (re-search-forward
            (rx " " (group (or (regexp "[]),.!?]") "\\}")))
            nil t)
      (replace-match "\\1"))

    (buffer-substring-no-properties (point-min) (point-max))))

(defun al/convert-latex-to-unicode (string)
  "Convert STRING containing LaTeX macros to unicode symbols."
  (with-temp-buffer
    (insert string)
    ;; If `case-fold-search' is t, then searching makes no
    ;; difference between "\alpha" and "\Alpha".
    (let (case-fold-search)
      (mapc (lambda (entry)
              (let* ((tex   (car entry))
                     (char  (cdr entry))
                     (subst (string char)))
                (goto-char (point-min))
                (while (search-forward tex nil t)
                  (replace-match subst))))
            tex--prettify-symbols-alist))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun al/copy-region-unicode-to-latex (beg end)
  "Save the region as if killed, converting unicode symbols to LaTeX.
This function is similar to `copy-region-as-kill' except it converts
some unicode symbols to LaTeX macros."
  (interactive
   (if (region-active-p)
       (let ((m (mark))
             (p (point)))
         (list (min m p) (max m p)))
     (user-error "Please, select some text")))
  (kill-new (al/convert-string-to-latex
             (buffer-substring-no-properties beg end)))
  (deactivate-mark))

;;;###autoload
(defun al/paste-region-unicode-to-latex ()
  "Paste the lastly killed text, converting unicode symbols to LaTeX."
  (interactive)
  (insert (al/convert-string-to-latex (current-kill 0))))

;;;###autoload
(defun al/copy-region-latex-to-unicode (beg end)
  "Save the region as if killed, converting LaTeX macros to unicode symbols."
  (interactive
   (if (region-active-p)
       (let ((m (mark))
             (p (point)))
         (list (min m p) (max m p)))
     (user-error "Please, select some text")))
  (kill-new (al/convert-latex-to-unicode
             (buffer-substring-no-properties beg end)))
  (deactivate-mark))

;;;###autoload
(defun al/paste-region-latex-to-unicode ()
  "Paste the lastly killed text, converting LaTeX macros to unicode."
  (interactive)
  (insert (al/convert-latex-to-unicode (current-kill 0))))

(provide 'al-tex)

;;; al-tex.el ends here
