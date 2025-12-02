;;; al-input-method.el --- Additional functionality for input methods  -*- lexical-binding: t -*-

;; Copyright © 2025 Alex Kost

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

(require 'seq)

(defvar al/default-input-methods
  '((text-mode . "al/utf"))
  "Alist of major modes and default input methods for them.")

;;;###autoload
(defun al/set-default-input-method ()
  "Set input method according to the current `major-mode'.
Input method is taken from `al/default-input-methods'.

If `major-mode' is not derived from one of the modes specified at
`al/default-input-methods', turn off the current input method."
  (let ((input-method
         (cdr (seq-find (lambda (assoc)
                          (derived-mode-p (car assoc)))
                        al/default-input-methods))))
    (unless (equal input-method current-input-method)
      ;; `al/set-default-input-method' is added to
      ;; `after-change-major-mode-hook' so make sure it is not failed
      ;; (in case input-method doesn't exist).
      (with-demoted-errors "ERROR during setting input method: %S"
        (set-input-method input-method)))))

;;;###autoload
(defun al/set-input-method (&optional input-method)
  "Activate input method INPUT-METHOD for the current buffer.
This is the same as `set-input-method', except it also handles
`isearch'."
  (interactive
   (let ((default (or (car input-method-history)
                      default-input-method)))
     (list (read-input-method-name
	    (format-prompt "Set input method" default)
	    default))))
  (set-input-method input-method)
  (if (null isearch-mode)
      (message "Current input method: %S." input-method)
    ;; The following settings are taken from `isearch-toggle-input-method'.
    (setq isearch-input-method-function input-method-function)
    ;; Without this line, `isearch' may exit after setting some input
    ;; methods, (in particular, "korean-hangul").
    (setq-local input-method-function nil)
    (isearch-update)))

(provide 'al-input-method)

;;; al-input-method.el ends here
