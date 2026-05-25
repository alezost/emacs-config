;;; al-dired.el --- Additional functionality for dired  -*- lexical-binding: t -*-

;; Copyright © 2012–2019 Alex Kost

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

(require 'dired)
(require 'al-process)


;;; Processes

;;;###autoload
(defun al/dired-start-process (program &optional args)
  "Open current file with a PROGRAM."
  ;; Shell command looks like this: "program [ARGS]... FILE" (ARGS can
  ;; be nil, so remove it).
  (apply #'al/start-process
         program
         (remove nil (list args (dired-get-file-for-visit)))))

;;;###autoload
(defun al/dired-start-process-on-marked-files (program &optional args)
  "Open marked files with a PROGRAM."
  (apply #'al/start-process
         program
         (remove nil (append args (dired-get-marked-files)))))


;;; Mode line

(defvar al/mode-info)

(defun al/dired-sort-set-mode-line ()
  "Replacement for `dired-sort-set-mode-line'."
  (when (eq major-mode 'dired-mode)
    (setq al/mode-info
	  (let (case-fold-search)
            (cond ((string-match-p
                    dired-sort-by-name-regexp dired-actual-switches)
                   "name")
                  ((string-match-p
                    dired-sort-by-date-regexp dired-actual-switches)
                   "date")
                  (t dired-actual-switches))))))


;;; "Ignored" files

(defvar al/dired-ignored-extensions nil
  "List of extensions to fontify with `dired-ignored' face in dired buffers.

This variable is used by `al/dired-set-completion-ignored-extensions' to
set buffer-local value of `completion-ignored-extensions'.

The problem with `completion-ignored-extensions' variable is that it is
used in 2 different cases:

  - to remove ignored files from completions for `find-file' and similar
    commands;

  - to fontify ignored files with `dired-ignored' face in dired buffers.")

(defun al/dired-set-completion-ignored-extensions ()
  "Set `completion-ignored-extensions' to `al/dired-ignored-extensions'."
  (setq-local completion-ignored-extensions al/dired-ignored-extensions))

(provide 'al-dired)

;;; al-dired.el ends here
