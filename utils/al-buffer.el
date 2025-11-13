;;; al-buffer.el --- Additional functionality for working with buffers  -*- lexical-binding: t -*-

;; Copyright © 2013–2025 Alex Kost

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

(defun al/kill-file-buffer (filename)
  "Kill the buffer visiting file FILENAME (a string).
Return nil, if there is no such live buffer."
  (let ((buffer (get-file-buffer filename)))
    (if buffer (kill-buffer buffer))))

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


;;; Getting buffers

(defun al/buffers (filter-pred &optional sort-pred)
  "Return a list of buffers satisfying FILTER-PRED predicate.
If SORT-PRED is specified, use this predicate to sort the list.
See `sort' for details."
  (let ((buffers (cl-remove-if-not
                  (lambda (buf) (funcall filter-pred buf))
                  (buffer-list))))
    (if sort-pred
        (sort buffers sort-pred)
      buffers)))

(defun al/buffers-by-regexp (regexp)
  "Return a list of buffers which names match REGEXP."
  (al/buffers
   (lambda (buf)
     (string-match-p regexp (buffer-name buf)))))

(defun al/buffers-by-mode (mode)
  "Return a list of buffers which `major-mode' is derived from MODE."
  (al/buffers
   (lambda (buf)
     (with-current-buffer buf
       (derived-mode-p mode)))))

(defun al/buffer-name< (b1 b2)
  "Call `string<' on names of buffers B1 and B2."
  (string< (buffer-name b1)
           (buffer-name b2)))


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


;;; Switching to previous buffers

(defvar al/switch-buffer-map (make-sparse-keymap))
(defvar al/original-buffer nil)
(defvar al/previous-buffers nil)
(defvar al/next-buffers nil)

(defvar al/skip-buffer-checkers
  '("\\` \\*Minibuf"
    get-buffer-window)
  "List of checkers for `al/skip-buffer'.
Each element should be either a function that takes a buffer as
its argument, or a string which is a regexp to match a buffer
name.")

(defun al/skip-buffer (buffer)
  "Return non-nil, if BUFFER should be ignored."
  (cl-find-if
   (lambda (checker)
     (cond
      ((stringp checker)
       (string-match-p checker (buffer-name buffer)))
      ((functionp checker)
       (funcall checker buffer))
      (t
       (message "Wrong checker: %s" checker)
       nil)))
   al/skip-buffer-checkers))

(defun al/switch-to-prev-buffer ()
  "Switch to previous buffer."
  (interactive)
  (let ((buf (pop al/previous-buffers)))
    (if (or (null buf)
            (al/skip-buffer buf))
        (al/switch-to-prev-buffer)
      (push (current-buffer) al/next-buffers)
      (switch-to-buffer buf)
      (set-transient-map al/switch-buffer-map))))

(defun al/switch-to-next-buffer ()
  "Switch to next buffer."
  (interactive)
  (let ((buf (pop al/next-buffers)))
    (if buf
        (progn
          (push (current-buffer) al/previous-buffers)
          (switch-to-buffer buf))
      (message "The first buffer is reached.")))
  (set-transient-map al/switch-buffer-map))

(defun al/switch-to-other-buffer ()
  "Switch between `al/original-buffer' and the current buffer ."
  (interactive)
  (if (eq (current-buffer) al/original-buffer)
      (al/switch-to-previous-buffer)
    (switch-to-buffer al/original-buffer)
    (set-transient-map al/switch-buffer-map)))

;;;###autoload
(defun al/switch-to-previous-buffer ()
  "Switch to the previously selected buffer.
This is similar to `mode-line-other-buffer' but with a transient
`al/switch-buffer-map' keymap."
  (interactive)
  (setq
   al/original-buffer (current-buffer)
   al/next-buffers nil
   al/previous-buffers (buffer-list))
  (al/switch-to-prev-buffer))


;;; Switching to some buffers

;;;###autoload
(defun al/display-buffer (buffer)
  "Switch to BUFFER, preferably reusing a window displaying this buffer."
  (pop-to-buffer buffer
                 '((display-buffer-reuse-window
                    display-buffer-same-window))))

;;;###autoload
(cl-defun al/switch-buffer (prompt &key buffers initial-input)
  "Switch to a buffer prompting with PROMPT for a buffer from BUFFERS.
If the list of BUFFERS is not specified, use all buffers.
See `completing-read' for the meaning of INITIAL-INPUT."
  (interactive (list "Switch to buffer: "))
  (let ((buffer-names (mapcar #'buffer-name
                              (or buffers (buffer-list)))))
    (switch-to-buffer
     (completing-read prompt buffer-names nil nil initial-input))))

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

;;;###autoload
(defun al/switch-to-faces ()
  "Switch to the buffer with packages."
  (interactive)
  (al/switch-to-buffer-or-funcall
   "*Faces*" #'list-faces-display))

(provide 'al-buffer)

;;; al-buffer.el ends here
