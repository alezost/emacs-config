;;; al-imenus.el --- Additional functionality for imenus

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

(require 'imenus)


;;; Searching/"imenu"-ing elisp files

(defvar al/imenus-elisp-dir user-emacs-directory
  "Directory used by `al/imenus-search-elisp-dir'.")

(defvar al/imenus-elisp-re "^[^.].*\\.el\\'"
  "Regexp for files to search from `al/imenus-elisp-dir'.")

(defvar al/imenus-elisp-prompt "Search elisp files: "
  "Prompt used by `al/imenus-search-elisp-dir'.")

;;;###autoload
(defun al/imenus-search-elisp-dir ()
  "Perform `imenus' on elisp files from `al/imenus-elisp-dir'."
  (interactive)
  (let ((files (directory-files al/imenus-elisp-dir
                                t al/imenus-elisp-re)))
    (imenus-files files nil al/imenus-elisp-prompt)))

(provide 'al-imenus)

;;; al-imenus.el ends here
