;;; al-buffer.el --- Additional functionality for working with buffers

;; Copyright © 2013–2018 Alex Kost

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

(defun al/kill-file-buffer (filename)
  "Kill the buffer visiting file FILENAME (a string).
Return nil, if there is no such live buffer."
  (let ((buffer (get-file-buffer filename)))
    (if buffer (kill-buffer buffer))))

(defun al/re-buffer-list (regexp)
  "Return a list of buffers which names match REGEXP."
  (let ((buffers))
    (dolist (buffer (buffer-list) buffers)
      (if (string-match-p regexp (buffer-name buffer))
          (setq buffers (cons buffer buffers))))))

;;;###autoload
(defun al/buffer-file-name (&optional buffer)
  "Return file name without extension for BUFFER.
If BUFFER is nil, use the current buffer.
If BUFFER is not visiting a file, return BUFFER name."
  (let ((file (buffer-file-name buffer)))
    (file-name-sans-extension
     (if file
         (file-name-nondirectory file)
       (buffer-name buffer)))))


;;; Putting buffer info into kill ring

(defun al/funcall-to-kill-ring (fun error-msg)
  "Call function FUN and put its result (string) into `kill-ring'.
Also display result string in minibuffer.
ERROR-MSG is a format string with one '%s' form, used as an error
message for a case when FUN does not return a string."
  (let ((out (funcall fun)))
    (or (stringp out)
        (error error-msg out))
    (kill-new out)
    (message out)))

;;;###autoload
(defun al/buffer-name-to-kill-ring ()
  "Put a name of the current buffer into `kill-ring'."
  (interactive)
  (al/funcall-to-kill-ring
   #'buffer-name "buffer-name has returned %s"))

;;;###autoload
(defun al/file-name-to-kill-ring ()
  "Put a name of the file visited by the current buffer into `kill-ring'."
  (interactive)
  (al/funcall-to-kill-ring
   #'buffer-file-name "buffer-file-name has returned %s"))

;;;###autoload
(defun al/major-mode-to-kill-ring ()
  "Put major mode name of the current buffer into `kill-ring'."
  (interactive)
  (al/funcall-to-kill-ring
   (lambda () (symbol-name major-mode)) "major-mode is %s"))

;;;###autoload
(defun al/default-directory-to-kill-ring ()
  "Put `default-directory' into `kill-ring'."
  (interactive)
  (al/funcall-to-kill-ring
   (lambda () default-directory) "%s"))


;;; Switching to some buffers

(require 'al-minibuffer)

;;;###autoload
(defun al/display-buffer (buffer)
  "Switch to BUFFER, preferably reusing a window displaying this buffer."
  (pop-to-buffer buffer
                 '((display-buffer-reuse-window
                    display-buffer-same-window))))

(defvar ivy-switch-buffer-map)
(defvar ido-default-buffer-method)
(declare-function ivy-read "ivy" t)
(declare-function ido-buffer-internal "ido" t)

;;;###autoload
(cl-defun al/switch-buffer (prompt &key buffers initial-input)
  "Switch to a buffer using `al/completing-read-engine'.
Specifying BUFFERS is not supported by `ido' engine."
  (interactive (list "Switch to buffer: "))
  (cond
   ((and (eq 'ivy al/completing-read-engine)
         (require 'ivy nil t))
    ;; Disable flx match, as I prefer to sort buffers chronologically.
    (let (ivy--flx-featurep)
      (ivy-read prompt
                (or buffers (mapcar #'buffer-name (buffer-list)))
                :initial-input initial-input
                :matcher 'ivy--switch-buffer-matcher
                :preselect (unless initial-input
                             (buffer-name (other-buffer (current-buffer))))
                :action 'ivy--switch-buffer-action
                :keymap ivy-switch-buffer-map
                :caller 'ivy-switch-buffer)))
   ((and (null buffers)
         (eq 'ido al/completing-read-engine)
         (require 'ido nil t))
    (ido-buffer-internal ido-default-buffer-method
                         nil prompt nil initial-input))
   (t
    (completing-read-default prompt buffers nil nil initial-input))))

(defun al/switch-to-buffer-or-funcall (buffer &optional function)
  "Switch to BUFFER or call FUNCTION.
BUFFER can be nil, a string, a buffer object or a function
returning one of those.  If there is no such buffer, call
FUNCTION if it is specified."
  (let* ((buffer (if (functionp buffer)
                     (funcall buffer)
                   buffer))
         (buffer (and buffer (get-buffer buffer))))
    (if buffer
        (switch-to-buffer buffer)
      (when function (funcall function)))))

;;;###autoload
(defun al/switch-to-characters (&optional charset)
  "Switch to the buffer with unicode characters from CHARSET.
If CHARSET is nil, use `unicode-bmp'.  With prefix, use `unicode-smp'."
  (interactive
   (list (and current-prefix-arg 'unicode-smp)))
  (al/switch-to-buffer-or-funcall
   "*Character List*"
   (lambda () (list-charset-chars (or charset 'unicode-bmp)))))

;;;###autoload
(defun al/switch-to-packages ()
  "Switch to the buffer with packages."
  (interactive)
  (al/switch-to-buffer-or-funcall
   "*Packages*" #'list-packages))

(provide 'al-buffer)

;;; al-buffer.el ends here
