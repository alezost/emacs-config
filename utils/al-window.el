;;; al-window.el --- Additional functionality for working with windows and frames  -*- lexical-binding: t -*-

;; Copyright © 2013–2017 Alex Kost

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


;;; Make 2 windows

;;;###autoload
(defun al/make-2-windows (&optional fun)
  "Make 2 windows in the current frame.
FUN is a function for splitting
windows (`split-window-vertically' by default)."
  (interactive)
  (or fun
      (setq fun 'split-window-below))
  (if (one-window-p)
      (funcall fun)
    (let ((cur-buffer (current-buffer)))
      (other-window -1)
      (delete-other-windows)
      (funcall fun)
      (switch-to-buffer cur-buffer))))

;;;###autoload
(defalias 'al/make-vertical-windows 'al/make-2-windows
  "Make 2 vertical windows.
If there is only one window, split it.
If there are more windows, show current and previous buffer in new
windows.")

;;;###autoload
(defun al/make-horizontal-windows ()
  "Make 2 horizontal windows.
If there is only one window, split it.
If there are more windows, show current and previous buffer in new
windows."
  (interactive)
  (al/make-2-windows 'split-window-right))


;;; Switching windows

(defvar al/other-window-order 1)

;;;###autoload
(defun al/other-window ()
  "Select the previously selected window."
  (interactive)
  (setq al/other-window-order (- al/other-window-order))
  (other-window al/other-window-order))

;;;###autoload
(defun al/switch-windows ()
  "Switch current and previous windows (show switched buffers)."
  (interactive)
  (unless (one-window-p)
    (let ((cur-buffer (current-buffer)))
      (other-window -1)
      (switch-to-buffer cur-buffer)
      (other-window 1)
      (switch-to-buffer nil)
      (other-window -1))))

;;;###autoload
(defun al/switch-or-next-window ()
  "Switch windows or select the next window.
If there are only 2 windows in the current frame, switch them,
otherwise select the next window."
  (interactive)
  (if (cddr (window-list))      ; > 2
      (other-window 1)
    (al/switch-windows)))

;;;###autoload
(defun al/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (let ((mb (active-minibuffer-window)))
    (if mb
        (select-window mb)
      (error "Minibuffer is not active"))))


;;; Frames

;;;###autoload
(defun al/maximize-frame (&optional _current-frame)
  "Maximize active frame using `wmctrl'.
The variable _CURRENT-FRAME affects nothing, it is used for
`after-make-frame-functions' (for maximizing new frames)."
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -b add,maximized_vert,maximized_horz"))


;; Update WINDOWS_NUM property for a stumpwm command, see
;; <https://github.com/alezost/stumpwm-config/blob/master/utils.lisp>.
;; Intended to be used with:
;; (add-hook 'window-configuration-change-hook 'al/set-windows-num-property)
;;;###autoload
(defun al/set-windows-num-property ()
  "Set X window property WINDOWS_NUM to the current number of windows."
  (and (display-graphic-p)
       (x-change-window-property
        "WINDOWS_NUM"
        (string (length (window-list)))
        nil nil nil t)))

(provide 'al-window)

;;; al-window.el ends here
