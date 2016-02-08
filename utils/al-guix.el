;;; al-guix.el --- Additional functionality for Guix

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 13 Oct 2015

;;; Code:

(require 'guix-devel)

;; This command was not accepted because it is «too convenient»:
;; <http://lists.gnu.org/archive/html/guix-devel/2015-10/msg00074.html>
;;;###autoload
(defun guix-devel-download-package-source ()
  "Download the source of the current package.
Use this function to compute SHA256 hash of the package source."
  (interactive)
  (guix-devel-with-definition def
    (guix-devel-use-modules "(guix packages)"
                            "(guix scripts download)")
    (when (or (not guix-operation-confirm)
              (y-or-n-p (format "Download '%s' package source?" def)))
      (guix-geiser-eval-in-repl
       (format "(guix-download (origin-uri (package-source %s)))"
               def)))))

;;;###autoload
(defun utl-guix-commit-url (commit)
  "Put to `kill-ring' and browse guix git repo URL for COMMIT."
  (interactive "sGuix commit: ")
  (let ((url (concat "http://git.savannah.gnu.org/cgit/guix.git/commit/?id="
                     commit)))
    (kill-new url)
    (browse-url url)))

(provide 'al-guix)

;;; al-guix.el ends here
