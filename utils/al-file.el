;;; al-file.el --- Additional functionality for working with files

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 8 Nov 2012

;;; Code:

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
