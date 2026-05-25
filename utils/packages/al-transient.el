;;; al-transient.el --- Additional functionality `transient'  -*- lexical-binding: t -*-

;; Copyright © 2026 Alex Kost

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

(require 'transient)

;; `transient' has an interesting bug with some input methods (in
;;  particular, with "Hangul"):
;;
;; - Switch to an editable buffer,
;;
;; - Call "M-x set-input-method RET korean-hangul RET",
;;
;; - Pop up any transient buffer (e.g., "M-x magit-dispatch"),
;;
;; - Press any letter key (e.g., "c").  Instead of running a transient
;;   command, this key (Hangul symbol) will be inserted in the current
;;   buffer.
;;
;; Interestingly, this bug "doesn't work" in a read-only buffer (e.g.,
;; in `magit-status' buffer).
;;
;; To fix this bug, I use the following functions to temporarily set
;; default input method before transient buffer pops up and restore the
;; original input method after transient exit.

(defvar al/transient-old-input-method nil
  "Input method saved before transient buffer pops up.
This input method is saved by `al/transient-fix-input-method' and
restored by `al/transient-restore-input-method'.")

(defvar al/transient-old-buffer nil
  "Buffer where `al/transient-old-input-method' was saved.")

(defun al/transient-fix-input-method (&rest _)
  "Save the current input method and deactivate it.
This function is intended to be used like so:

  (advice-add \\='transient-setup :before #\\='al/transient-fix-input-method)"
  (when current-input-method
    (setq al/transient-old-input-method current-input-method
          al/transient-old-buffer (current-buffer))
    (set-input-method nil)))

(defun al/transient-restore-input-method (&rest _)
  "Restore the last input method.
This function is intended to be used like so:

  (add-hook \\='transient-post-exit-hook #\\='al/transient-restore-input-method)"
  (when al/transient-old-input-method
    (when (buffer-live-p al/transient-old-buffer)
      (with-current-buffer al/transient-old-buffer
        (set-input-method al/transient-old-input-method)))
    (setq al/transient-old-input-method nil
          al/transient-old-buffer nil)))

(provide 'al-transient)

;;; al-transient.el ends here
