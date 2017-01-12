;;; al-minibuffer.el --- Additional functionality for minibuffer

;; Copyright © 2013–2017 Alex Kost

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

;; Idea to use a custom completing-read function with the possibility to
;; fallback to `completing-read-default' came from
;; <http://www.emacswiki.org/emacs/InteractivelyDoThings#toc15>.

(defvar al/completing-read-engine 'ido
  "Engine used by `al/completing-read'.
Can be either `ido' or nil (to fallback to
`completing-read-default').")

(defun al/completing-read (prompt collection &optional predicate
                                  require-match initial-input
                                  hist def inherit-input-method)
  "Function for `completing-read-function' variable.
Use completion engine depending on `al/completing-read-engine'."
  ;; Match is never required in the following calls, otherwise it's not
  ;; possible to select "#XXXXXX" with `read-color'.
  (cl-case al/completing-read-engine
    (ido
     (ido-completing-read prompt (all-completions "" collection predicate)
                          nil nil initial-input hist def))
    (t (completing-read-default prompt collection predicate
                                nil initial-input
                                hist def inherit-input-method))))

(defun al/complete-default (fun &rest args)
  "Use `completing-read-default' for FUN.
This function is intended to be used as an 'around' advice for
FUN, for example:

  (advice-add 'org-set-tags :around #'al/complete-default)"
  (let (al/completing-read-engine)
    (apply fun args)))

(provide 'al-minibuffer)

;;; al-minibuffer.el ends here
