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


;;; Commands that use `al/minibuffer-fallback'

(defun al/minibuffer-replace-and-exit (string)
  "Replace the current minibuffer input with STRING and exit."
    ;; This procedure exists because `exit-minibuffer' exits with the
    ;; current (most likely partial) input and it is not what the
    ;; current interactive command expects, so we need to insert a
    ;; correct input.  We can do this with
    ;; `minibuffer-force-complete-and-exit' but it is very slow.
    ;; Instead the current completion is passed as STRING to this
    ;; procedure.
  (if (not (window-minibuffer-p))
      (error "Not in minibuffer")
    (delete-region (minibuffer-prompt-end) (point-max))
    (insert string)
    (exit-minibuffer)))

;;;###autoload
(defun al/minibuffer-find-symbol ()
  "Display documentation of the current minibuffer completion."
  (interactive)
  (let* ((symbol-name (al/minibuffer-current-completion))
         (symbol (intern symbol-name)))
    (setq al/minibuffer-fallback
          (lambda ()
            (cond
             ((fboundp symbol) (find-function symbol))
             ((boundp symbol)  (find-variable symbol))
             ((facep symbol)   (find-face-definition symbol)))))
    (al/minibuffer-replace-and-exit symbol-name)))

;;;###autoload
(defun al/minibuffer-describe-symbol ()
  "Display documentation of the current minibuffer completion."
  (interactive)
  (let* ((symbol-name (al/minibuffer-current-completion))
         (symbol (intern symbol-name)))
    (setq al/minibuffer-fallback
          (lambda () (describe-symbol symbol)))
    (al/minibuffer-replace-and-exit symbol-name)))

(declare-function magit-toplevel "magit-git")
(declare-function magit-status "magit-status")

;;;###autoload
(defun al/minibuffer-enter-magit-status ()
  "Enter magit status buffer for the directory in minibuffer."
  (interactive)
  (require 'magit)
  (let ((dir (magit-toplevel
              (file-name-directory (minibuffer-contents)))))
    (setq al/minibuffer-fallback
          (lambda () (magit-status dir)))
    (al/minibuffer-replace-and-exit "/")))

(provide 'al-minibuffer-cmd)

;;; al-minibuffer-cmd.el ends here
