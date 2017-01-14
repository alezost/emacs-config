;;; al-file-cmd.el --- Interactive commands for working with files

;; Copyright © 2012–2017 Alex Kost

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

(declare-function counsel-find-file "counsel" t)

;;;###autoload
(defun al/find-file (&optional dir)
  "Find file starting from DIR if it is non-nil.
Call either `counsel-find-file' or `ido-find-file'."
  (interactive)
  (let ((default-directory (file-name-as-directory
                            (or dir default-directory))))
    (if (fboundp 'counsel-find-file)
        (counsel-find-file)
      (ido-find-file))))

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


;;; Files in PATH

(defvar al/path-completions nil
  "List of names of executable files from PATH.")

(defun al/refresh-path-completions ()
  "Refresh `al/path-completions'."
  (interactive)
  (let ((regexp (rx "." (zero-or-one ".") "/")))
    (setq al/path-completions
          (sort (locate-file-completion-table
                 exec-path exec-suffixes ""
                 (lambda (name)
                   (not (string-match-p regexp name)))
                 t)
                #'string<))))

;;;###autoload
(defun al/find-file-in-path (file)
  "Edit executable FILE found in PATH environment variable."
  (interactive
   (progn
     (unless al/path-completions
       (al/refresh-path-completions))
     (list (completing-read "Find PATH file: "
                            al/path-completions))))
  (let ((file (executable-find file)))
    (when file
      (find-file file))))

(provide 'al-file-cmd)

;;; al-file-cmd.el ends here
