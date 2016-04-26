;;; al-mode-line.el --- Additional functionality for mode-line

;; Copyright © 2014-2016 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


;;; Mode names

;; Idea from
;; <http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/>.

(defvar al/mode-names-alist nil
  "Alist of mode names.
Car of each assoc is a `major-mode'.  Cdr is a string or a
function returning a string used for `mode-name'.")

;;;###autoload
(defun al/mode-name (&rest _)
  "Replace `mode-name' of the current major mode.
Use the appropriate name from `al/mode-names-alist'.
This function is intended to be used in `after-change-major-mode-hook'."
  (interactive)
  (let ((name (cdr (assq major-mode al/mode-names-alist))))
    (when name
      (setq mode-name
            (if (functionp name) (funcall name) name)))))

;;;###autoload
(defun al/mode-line-default-buffer-identification (mode)
  "Set `mode-line-buffer-identification' to the default value for MODE.
Some major modes like to override
`mode-line-buffer-identification'.  If you want to force a mode
to use the default value, call this function like this:

  (al/mode-line-default-buffer-identification 'Man-mode)
  (al/mode-line-default-buffer-identification 'dired-mode)"
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook
              (lambda ()
                (setq mode-line-buffer-identification
                      (default-value 'mode-line-buffer-identification))))))


;;; Mode line process

(defvar al/mode-line-process '("[%s]")
  "String used in `al/mode-line-process' function.")

;;;###autoload
(defun al/mode-line-process ()
  "Set `mode-line-process' to the value of `al/mode-line-process'.
This function is intended to be used in hooks:

  (add-hook 'comint-mode-hook 'al/mode-line-process)"
  (setq mode-line-process al/mode-line-process))


;;; Additional info for major modes

;; To see some additional info in the mode line, I add `al/mode-info'
;; to the `mode-line-modes'.

(defvar-local al/mode-info nil
  "Part of mode line with additional info for the current major mode.")
(put 'al/mode-info 'risky-local-variable t)

;;;###autoload
(defun al/mode-ibuffer-info ()
  "Set `al/mode-info' to the additional info for `ibuffer-mode'.
This function is intended to be added to `ibuffer-mode-hook'."
  (setq al/mode-info
        '(""
          (ibuffer-sorting-mode (:eval (symbol-name ibuffer-sorting-mode)))
          (ibuffer-sorting-reversep "|r"))))

(provide 'al-mode-line)

;;; al-mode-line.el ends here
