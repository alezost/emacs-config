;;; al-minibuffer-cmd.el --- Various interactive commands for minibuffer  -*- lexical-binding: t -*-

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

(require 'al-minibuffer)

(defun al/minibuffer-current-completion ()
  "Return the current minibuffer completion."
  (car (completion-all-sorted-completions)))

;;;###autoload
(defun al/minibuffer-copy-current-completion ()
  "Put the current minibuffer completion into `kill-ring'."
  (interactive)
  (kill-new (al/minibuffer-current-completion)))

;;;###autoload
(defun al/minibuffer-set-directory (dir)
  "Change the current directory in minibuffer prompt to DIR."
  (kill-whole-line)
  (insert (file-name-as-directory (expand-file-name dir))))

;;;###autoload
(defun al/describe-variable ()
  "Call `describe-variable' with `al/minibuffer-symbol-map'.
This function exists because adding `al/read-symbol-add-keymap' advice
to `describe-variable' will not work, since it reads symbol from the
`interactive' call directly."
  (interactive)
  (al/minibuffer-with-keymap al/minibuffer-symbol-map
    (call-interactively #'describe-variable)))

;;;###autoload
(defun al/describe-symbol ()
  "Call `describe-symbol' with `al/minibuffer-symbol-map'.
See also `al/describe-variable'."
  (interactive)
  (al/minibuffer-with-keymap al/minibuffer-symbol-map
    (call-interactively #'describe-symbol)))

(provide 'al-minibuffer-cmd)

;;; al-minibuffer-cmd.el ends here
