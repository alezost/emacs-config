;;; al-imenus.el --- Additional functionality for imenus

;; Copyright © 2014–2017, 2020 Alex Kost

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

(require 'cl-lib)
(require 'imenus)
(require 'al-minibuffer)

(declare-function ivy-read "ivy" t)

(defun al/imenus-completing-read (prompt collection &optional predicate
                                         require-match initial-input
                                         history def inherit-input-method)
  "Function for `imenus-completing-read-function'.
This is like `completing-read' but with supporting sorting
candidates for `ivy'."
  (cond
   ((and (eq 'ivy al/completing-read-engine)
         (require 'ivy nil t))
    (let ((input-method (and inherit-input-method
                             current-input-method)))
      ;; See the commentary for `al/completing-read'.
      (minibuffer-with-setup-hook
          (lambda () (set-input-method input-method))
        ;; Disable flx match to make `ivy--sort' sort candidates using
        ;; `ivy-sort-matches-functions-alist'.
        (let (ivy--flx-featurep)
          (ivy-read prompt collection
                    :preselect (thing-at-point 'symbol)
                    :initial-input initial-input
                    :history history
                    :caller 'imenus)))))
   (t
    (funcall completing-read-function
             prompt collection predicate require-match
             initial-input history def inherit-input-method))))


;;; Searching/"imenu"-ing elisp files

(defvar al/imenus-elisp-directories (list user-emacs-directory)
  "List of directories used by `al/imenus-search-elisp-directories'.")

(defvar al/imenus-elisp-re "^[^.].*\\.el\\'"
  "Regexp for files to search in `al/imenus-elisp-directories'.")

(defvar al/imenus-elisp-prompt "Search elisp files: "
  "Prompt used by `al/imenus-search-elisp-directories'.")

;;;###autoload
(defun al/imenus-search-elisp-directories ()
  "Perform `imenus' on elisp files from `al/imenus-elisp-directories'."
  (interactive)
  (let ((files (cl-mapcan (lambda (dir)
                            (directory-files dir t al/imenus-elisp-re))
                          al/imenus-elisp-directories)))
    (imenus-files files nil al/imenus-elisp-prompt)))

(provide 'al-imenus)

;;; al-imenus.el ends here
