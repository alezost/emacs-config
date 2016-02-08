;;; al-buffer.el --- Additional functionality for working with buffers

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 10 Aug 2013

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

(defun al/switch-to-buffer-or-funcall (buffer &optional fun)
  "Switch to the buffer BUFFER-OR-NAME or call a function FUN.
BUFFER-OR-NAME can be a string, a buffer object or a function
returning one of those.  If there is no such buffer, call a
function FUN if it is specified."
  (let ((buf (if (functionp buffer)
                 (funcall buffer)
               buffer)))
    (if (and buf
             (setq buf (get-buffer buf)))
        (switch-to-buffer buf)
      (and fun (funcall fun)))))

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

(declare-function w3m "w3m" t)

;;;###autoload
(defun al/switch-to-w3m ()
  "Switch to the `w3m' buffer."
  (interactive)
  (al/switch-to-buffer-or-funcall
   (lambda ()
     (if (fboundp 'w3m-alive-p)
         (w3m-alive-p)
       (error "w3m is not running")))
   #'w3m))

(defvar aurel-list-buffer-name)
(defvar aurel-info-buffer-name)

;;;###autoload
(defun al/switch-to-aurel-list ()
  "Switch to the `aurel-list-buffer-name' buffer."
  (interactive)
  (al/switch-to-buffer-or-funcall
   aurel-list-buffer-name
   (lambda () (call-interactively 'aurel-package-search))))

;;;###autoload
(defun al/switch-to-aurel-info ()
  "Switch to the `aurel-info-buffer-name' buffer."
  (interactive)
  (al/switch-to-buffer-or-funcall
   aurel-info-buffer-name
   (lambda () (call-interactively 'aurel-package-info))))

(provide 'al-buffer)

;;; al-buffer.el ends here
