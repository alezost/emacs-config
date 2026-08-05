;;; al-buffer.el --- Additional functionality for working with buffers  -*- lexical-binding: t -*-

;; Copyright © 2013–2026 Alex Kost

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

(eval-when-compile
  (require 'cl-lib)
  (require 'fp-utils)
  (require 'let-macros))
(require 'seq)
(require 'al-general)

(defun al/buffer-derived-mode? (buffer &rest modes)
  "Return non-nil if BUFFER major mode is derived from one of MODES."
  (with-current-buffer buffer
    (derived-mode-p modes)))


;;; Getting buffers

(defun al/buffers (&optional filter-pred sort-pred)
  "Return a list of buffers satisfying FILTER-PRED predicate if specified.
If SORT-PRED is specified, use this predicate to sort the list.
See `sort' for details."
  (let ((buffers (if filter-pred
                     (seq-filter filter-pred (buffer-list))
                   (buffer-list))))
    (if sort-pred
        (sort buffers sort-pred)
      buffers)))

(defun al/buffers-by-regexp (regexp &optional sort-pred)
  "Return a list of buffers which names match REGEXP.
See `al/buffers' for the meaning of SORT-PRED."
  (al/buffers
   (lambda (buf)
     (string-match-p regexp (buffer-name buf)))
   sort-pred))

(defun al/buffers-by-mode (mode &optional sort-pred)
  "Return a list of buffers which `major-mode' is derived from MODE.
See `al/buffers' for the meaning of SORT-PRED."
  (al/buffers (cut #'al/buffer-derived-mode? <> mode)
              sort-pred))

(defun al/buffer-name< (b1 b2)
  "Call `string<' on names of buffers B1 and B2."
  (string< (buffer-name b1)
           (buffer-name b2)))


;;; Switching to some buffers

(defmacro al/with-pop-to-other-window (&rest body)
  "Display buffer popped by evaluating BODY in other window."
  (declare (indent 0) (debug t))
  `(let ((display-buffer-overriding-action
          '((display-buffer-pop-up-window)
            (inhibit-same-window . t))))
     ,@body))

(defmacro al/with-pop-to-current-window (&rest body)
  "Display buffer popped by evaluating BODY in current window."
  (declare (indent 0) (debug t))
  `(let ((display-buffer-overriding-action
          '((display-buffer-same-window))))
     ,@body))

(defmacro al/with-pop-to-default-window (&rest body)
  "Display buffer popped by evaluating BODY in default window.
Default means current window except if some window already displays the
popped buffer, reuse it."
  (declare (indent 0) (debug t))
  `(let ((display-buffer-overriding-action
          '((display-buffer-reuse-window
             display-buffer-same-window))))
     ,@body))

;;;###autoload
(defun al/display-buffer (buffer)
  "Switch to BUFFER, preferably reusing a window displaying this buffer."
  (al/with-pop-to-default-window
    (pop-to-buffer buffer)))

;;;###autoload
(defun al/display-buffer-other-window (buffer)
  "Switch to BUFFER, preferably in other window."
  (al/with-pop-to-other-window
    (pop-to-buffer buffer)))

(cl-defun al/switch-buffer (&key prompt buffers initial-input)
  "Switch to a buffer prompting with PROMPT for a buffer from BUFFERS.
If the list of BUFFERS is not specified, use all buffers.
See `completing-read' for the meaning of INITIAL-INPUT."
  (let ((prompt (or prompt "Switch to buffer: "))
        (buffer-names (mapcar #'buffer-name
                              (or buffers (buffer-list)))))
    (al/display-buffer
     (completing-read prompt buffer-names nil nil initial-input))))

(defun al/rotate-or-select-buffer (buffers &optional fallback select)
  "Switch to buffer from BUFFERS.

BUFFERS can be a list of buffers or a function returning such list.

If BUFFERS is nil, call FALLBACK function.  Alternatively, FALLBACK can
be a string.  In this case, show message with this string.

Buffer for switching is defined as the next buffer after the current one
in BUFFERS.  If current buffer is not in the list, switch to the first
buffer.

If SELECT is non-nil, prompt for buffer to switch in the minibuffer
instead of automatic switching.  If SELECT is a string, use it as the
prompt string."
  (let ((buffers
         (cond ((listp     buffers) buffers)
               ((functionp buffers) (funcall buffers))
               (t (error "Unknown buffers: %S" buffers)))))
    (pcase buffers
      ('()
       (cond
        ((functionp fallback) (funcall fallback))
        ((stringp   fallback) (message fallback))))
      (`(,buf)
       (unless (eq buf (current-buffer))
         (al/display-buffer buf)))
      (_
       (if select
           (al/switch-buffer :prompt (and (stringp select) select)
                             :buffers buffers)
         (al/with-pop-to-current-window
           (pop-to-buffer (al/next-element buffers (current-buffer)))))))))

(defun al/switch-to-buffer-or-funcall (buffer &optional function)
  "Switch to BUFFER or call FUNCTION.
BUFFER can be nil, a string, a buffer object or a function
returning one of those.  If there is no such buffer, call
FUNCTION if it is specified."
  (if-let ((buffer (if (functionp buffer)
                       (funcall buffer)
                     buffer))
           (buffer (get-buffer buffer)))
      (al/display-buffer buffer)
    (when function (funcall function))))

(provide 'al-buffer)

;;; al-buffer.el ends here
