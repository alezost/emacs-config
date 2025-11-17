;;; al-process.el --- Additional functionality for working with processs  -*- lexical-binding: t -*-

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

(defun al/start-process (program &rest args)
  "Same as `start-process', but don't bother about name and buffer."
  (let ((process-name (concat program "_process"))
        (buffer-name  (generate-new-buffer-name
                       (concat program "_output"))))
    (apply #'start-process
           process-name buffer-name program args)))

(defvar al/process-locale "C"
  "Default locale for `al/call-process'.")

(defun al/call-with-locale (fun &rest args)
  "Call FUN with ARGS using `al/process-locale'.
This function can be used as an `around' advice.
For example, `insert-directory' (used by `dired') calls
`insert-directory-program' (\"ls -l\" more or less) and searches
for \"total\" in its output to insert occupied and free disk
space.  But \"total\" is obviously not available for non-English
locales.  The following line should fix this problem.

  (advice-add \\='insert-directory :around #\\='al/call-with-locale)
"
  (let ((process-environment
         (cons (concat "LC_ALL=" al/process-locale)
               process-environment)))
    (apply fun args)))

(defun al/call-process (program &optional infile destination display
                                &rest args)
  "Same as `call-process', but using `al/process-locale' instead
of the current locale."
  (apply #'al/call-with-locale
         #'call-process program infile destination display args))

;; Idea from
;; <http://stackoverflow.com/questions/11572934/how-do-i-kill-a-running-process-in-emacs>.

;;;###autoload
(defun al/kill-process (process)
  "Kill PROCESS.
See `delete-process' for the meaning of PROCESS.
Interactively prompt for PROCESS name."
  (interactive
   (list (get-process (completing-read
                       "Kill process: "
                       (mapcar #'process-name (process-list))))))
  (delete-process process))

(defun al/process-is-program (args name)
  "Return non-nil, if process defined by ARGS has program NAME."
  (let ((prog (car args)))
    (when prog
      (or (string= prog name)
          (and (string-match-p "sh\\'" prog) ; if it is bash/sh/...
               (string= (nth 1 args) "-c")
               (string-match-p (regexp-quote name)
                               (nth 2 args)))))))


;; Hooks for starting/calling processes

(defvar al/before-process-functions '(al/process-message)
  "Functions to be called before Emacs starts an external process.
Each function is called by applying to ARGS.  The first element
of ARGS is a program name of the process, and the rest are
program arguments.")

(defun al/process-message (&rest args)
  "Display message about ARGS."
  (message "Process to run: %S" args))

(defun al/run-before-process-hook (program args)
  "Run `al/before-process-functions' using PROGRAM and ARGS."
  (apply #'run-hook-with-args
         'al/before-process-functions program args))

(defun al/run-before-call-process-hook
    (program &optional _infile _destination _display &rest args)
  "Run `al/before-process-functions' using PROGRAM and ARGS."
  (al/run-before-process-hook program args))

(defun al/run-before-start-process-hook
    (_name _buffer program &rest args)
  "Run `al/before-process-functions' using PROGRAM and ARGS."
  (al/run-before-process-hook program args))

;;;###autoload
(defun al/enable-process-hooks ()
  "Make `al/before-process-functions' active."
  (interactive)
  (advice-add 'call-process :before #'al/run-before-call-process-hook)
  (advice-add 'start-process :before #'al/run-before-start-process-hook))

;;;###autoload
(defun al/disable-process-hooks ()
  "Make `al/before-process-functions' inactive."
  (interactive)
  (advice-remove 'call-process #'al/run-before-call-process-hook)
  (advice-remove 'start-process #'al/run-before-start-process-hook))

(provide 'al-process)

;;; al-process.el ends here
