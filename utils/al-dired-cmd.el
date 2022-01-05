;;; al-dired-cmd.el --- Additional commands for dired

;; Copyright © 2012–2021 Alex Kost

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

(require 'dired)
(require 'dired-x)
(require 'al-file)
(require 'al-process)


;;; Navigating by files

;; Moving to the first/last files - from <http://whattheemacsd.com/>.

;;;###autoload
(defun al/dired-beginning-of-buffer ()
  "Move point to the first file."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 4))

;;;###autoload
(defun al/dired-end-of-buffer ()
  "Move point to the last file."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))


;;; Opening files

;;;###autoload
(defun al/dired-find-file (&optional arg)
  "In Dired, visit the file or directory named on this line.
If ARG is non-nil, visit it with `find-file-literally'."
  (interactive "P")
  (if arg
      (find-file-literally (dired-get-file-for-visit))
    (dired-find-file)))

(declare-function browse-url-file-url "browse-url" (file))

;;;###autoload
(defun al/dired-browse-url ()
  "Open the current file in browser."
  (interactive)
  (require 'browse-url)
  (browse-url (browse-url-file-url (dired-get-file-for-visit))))

;;;###autoload
(defun al/dired-append-marked-files (file-name &optional arg)
  "Insert the contents of the marked files into FILE-NAME.
If ARG is non-nil, insert a file name before the contents of each
file."
  (interactive
   (if (derived-mode-p 'dired-mode)
       (list (expand-file-name (read-file-name "Write to file: "))
             current-prefix-arg)
     (error "Not a Dired buffer")))
  (al/append-files (dired-get-marked-files) file-name arg))


;;; File stats

;;;###autoload
(defun al/dired-get-size (&optional arg)
  "Show size of all marked files in dired mode.
If ARG is non-nil, do not use human readable format (size in bytes)."
  (interactive "P")
  (let ((args  (concat "-sc"
                       (if arg "b" "h")))
        (files (dired-get-marked-files)))
    (with-temp-buffer
      (apply #'al/call-process "du" nil t nil args files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^.+\\)[[:blank:]]*total$")
                 (match-string 1))))))

;;;###autoload
(defun al/dired-stat (&optional arg)
  "Call `stat' program on marked files in dired mode.
With prefix (if ARG is non-nil), use the next ARG files instead."
  (interactive "P")
  (dired-do-shell-command
   "stat" nil
   (dired-get-marked-files t arg)))


;;; File names

;;;###autoload
(defun al/dired-copy-filename-as-kill (&optional arg)
  "Copy names of marked files into the kill ring.
This function is similar to `dired-copy-filename-as-kill',
except it quotes file names for a shell, unless ARG is non-nil."
  (interactive "P")
  (let ((string (mapconcat
                 (if arg #'identity #'shell-quote-argument)
                 (dired-get-marked-files)
                 " ")))
    (kill-new string)
    (message "%s" string)))


;;; Miscellaneous commands

(defun al/man-file-p (file)
  "Return non-nil, if FILE is a Man-file."
  ;; Is there a more reliable way?
  (string-match-p (rx "." (any "0-8") (? ".gz") string-end)
                  file))

;;;###autoload
(defun al/dired-man-or-chmod ()
  "Perform `dired-man' on a Man-file or `dired-do-chmod' otherwise."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (al/man-file-p file)
        (dired-man)
      (dired-do-chmod))))

(declare-function image-dired-backward-image
                  "image-dired" (&optional arg))
(declare-function image-dired-modify-mark-on-thumb-original-file
                  "image-dired" (command))

;;;###autoload
(defun al/image-dired-unmark-thumb-original-file-backward ()
  "Move up and unmark original image file in associated dired buffer."
  (interactive)
  (image-dired-backward-image)
  (image-dired-modify-mark-on-thumb-original-file 'unmark))

(provide 'al-dired-cmd)

;;; al-dired-cmd.el ends here
