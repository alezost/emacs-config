;;; al-file.el --- Additional functionality for working with files

;; Copyright © 2012-2016 Alex Kost

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

(defun al/add-to-auto-mode-alist (specs)
  "Add SPECS to `auto-mode-alist'.
Each specification from SPECS list may have one of the following forms:

  (MODE-NAME . REGEXPS)
  (MODE-NAME REGEXP-OR-LIST t)

REGEXP-OR-LIST is either a regexp (string), or a list of regexps.
For the first form, specifications are added at the beginning of
`auto-mode-alist'; for the second form it is added at the end."
  (cl-flet ((add (mode regexp &optional append?)
              (add-to-list 'auto-mode-alist (cons regexp mode) append?)))
    (dolist (spec specs)
      (pcase spec
        (`(,mode ,str-or-lst t)
         (if (stringp str-or-lst)
             (add mode str-or-lst t)
           (dolist (regexp str-or-lst)
             (add mode regexp t))))
        (`(,mode . ,regexps)
         (dolist (regexp regexps)
           (add mode regexp)))))))

;;;###autoload
(defun al/ido-find-file (&optional dir)
  "Similar to `ido-find-file', but start from DIR if it is non-nil."
  (interactive)
  (let ((default-directory (or dir default-directory)))
    (ido-find-file)))

;;;###autoload
(defun al/sudo-find-file (&optional arg)
  "Find current file or dired directory with root privileges.
If ARG is nil use `find-alternate-file', otherwise - `find-file'."
  (interactive "P")
  (let ((file (or (and (eq major-mode 'dired-mode)
                       dired-directory)
                  buffer-file-name
                  (error "Current buffer should visit a file or be in a dired-mode")))
        (window-start (window-start))
        (point (point))
        (mark (and mark-active (region-beginning))))
    (funcall (if arg #'find-file #'find-alternate-file)
             (format "/sudo::%s" file))
    (and mark (set-mark mark))
    (goto-char point)
    (set-window-start nil window-start)))

(defvar al/ssh-default-user user-login-name
  "A default user name for `al/ssh-find-file'.
Can be a string or a list of strings (names).")
(defvar al/ssh-default-host "remote-host"
  "A default host name for `al/ssh-find-file'.
Can be a string or a list of strings (hosts).")

;;;###autoload
(defun al/ssh-find-file (&optional user host)
  "Find a file for a USER on a HOST using tramp ssh method.
If USER and HOST are not specified, values from
`al/ssh-default-user' and `al/ssh-default-host' will be used.
Interactively with \\[universal-argument] prompt for a user name,
with \\[universal-argument] \\[universal-argument] prompt for a default host as well."
  (interactive
   (list (and (or (equal current-prefix-arg '(4))
                  (equal current-prefix-arg '(16)))
              (ido-completing-read "User: "
                                   (if (listp al/ssh-default-user)
                                       al/ssh-default-user
                                     (list al/ssh-default-user))))
         (and (equal current-prefix-arg '(16))
              (ido-completing-read "Host: "
                                   (if (listp al/ssh-default-host)
                                       al/ssh-default-host
                                     (list al/ssh-default-host))))))
  (or user (setq user (or (and (listp al/ssh-default-user)
                               (car al/ssh-default-user))
                          al/ssh-default-user)))
  (or host (setq host (or (and (listp al/ssh-default-host)
                               (car al/ssh-default-host))
                          al/ssh-default-host)))
  (with-current-buffer
      (find-file-noselect (format "/ssh:%s@%s:/" user host))
    (ido-find-file)))


;;; Backup

;; Setting `make-backup-file-name-function' is not enough as it is used
;; by `make-backup-file-name', but not by `find-backup-file-name', so
;; replace `make-backup-file-name-1' instead.

(defun al/make-backup-file-name-1 (file)
  "Return a new backup file path of a given FILE.
If the new path's directories do not exist, create them.
This function is intended to be used as a substitution for
`make-backup-file-name-1'."
  (let ((alist backup-directory-alist)
        (file (expand-file-name file))
	elt backup-directory abs-backup-directory backup-file)
    (while alist
      (setq elt (pop alist))
      (if (string-match (car elt) file)
	  (setq backup-directory (cdr elt)
		alist nil)))
    (if (null backup-directory)
        (setq backup-file file)
      (setq backup-file
            ;; New full path in backup dir tree.
            (concat (directory-file-name (expand-file-name backup-directory))
                    file)
            abs-backup-directory (file-name-directory backup-file))
      (if (and abs-backup-directory
               (not (file-exists-p abs-backup-directory)))
          (condition-case nil
              (make-directory abs-backup-directory 'parents)
            (file-error (setq backup-directory nil
                              abs-backup-directory nil)))))
    backup-file))

(defun al/backup-enable-predicate (name)
  "Function for `backup-enable-predicate'.
Do not backup su/sudo files."
  (and (normal-backup-enable-predicate name)
       (not (let ((method (file-remote-p name 'method)))
              (when (stringp method)
                (member method '("su" "sudo")))))))

(provide 'al-file)

;;; al-file.el ends here
