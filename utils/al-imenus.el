;;; al-imenus.el --- Additional functionality for imenus

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 20 Dec 2014

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
