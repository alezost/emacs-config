;;; utl-imenus.el --- Additional functionality for imenus

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 20 Dec 2014

;;; Code:

(require 'imenus)


;;; Searching/"imenu"-ing elisp files

(defvar utl-imenus-elisp-dir user-emacs-directory
  "Directory used by `utl-imenus-search-elisp-dir'.")

(defvar utl-imenus-elisp-re "^[^.].*\\.el\\'"
  "Regexp for files to search from `utl-imenus-elisp-dir'.")

(defvar utl-imenus-elisp-prompt "Search elisp files: "
  "Prompt used by `utl-imenus-search-elisp-dir'.")

;;;###autoload
(defun utl-imenus-search-elisp-dir ()
  "Perform `imenus' on elisp files from `utl-imenus-elisp-dir'."
  (interactive)
  (let ((files (directory-files utl-imenus-elisp-dir
                                t utl-imenus-elisp-re)))
    (imenus-files files nil utl-imenus-elisp-prompt)))

(provide 'utl-imenus)

;;; utl-imenus.el ends here
