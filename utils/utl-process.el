;;; utl-process.el --- Additional functionality for working with processs

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 10 Aug 2013

;;; Code:

(require 'cl-lib)

(defun utl-start-process (program &rest args)
  "Same as `start-process', but don't bother about name and buffer."
  (let ((process-name (concat program "_process"))
        (buffer-name  (generate-new-buffer-name
                       (concat program "_output"))))
    (apply #'start-process
           process-name buffer-name program args)))

;; Idea from
;; <http://stackoverflow.com/questions/11572934/how-do-i-kill-a-running-process-in-emacs>.

;;;###autoload
(defun utl-kill-process (process)
  "Kill PROCESS.
See `delete-process' for the meaning of PROCESS.
Interactively prompt for PROCESS name."
  (interactive
   (list (get-process (completing-read
                       "Kill process: "
                       (mapcar #'process-name (process-list))))))
  (delete-process process))

(defun utl-process-is-program (args name)
  "Return non-nil, if process defined by ARGS has program NAME."
  (let ((prog (car args)))
    (or (string= prog name)
        (and (string-match-p "sh\\'" prog) ; if it is bash/sh/...
             (string= (cl-second args) "-c")
             (string-match-p (regexp-quote name)
                             (cl-third args))))))


;; Hooks for starting/calling processes

(defvar utl-before-process-functions '(utl-process-message)
  "Functions to be called before Emacs starts an external process.
Each function is called by applying to ARGS.  The first element
of ARGS is a program name of the process, and the rest are
program arguments.")

(defun utl-process-message (&rest args)
  "Display message about ARGS."
  (message "Process to run: %S" args))

(defun utl-run-before-process-hook (program args)
  "Run `utl-before-process-functions' using PROGRAM and ARGS."
  (apply #'run-hook-with-args
         'utl-before-process-functions program args))

(defun utl-run-before-call-process-hook
    (program &optional infile destination display &rest args)
  "Run `utl-before-process-functions' using PROGRAM and ARGS."
  (utl-run-before-process-hook program args))

(defun utl-run-before-start-process-hook
    (name buffer program &rest args)
  "Run `utl-before-process-functions' using PROGRAM and ARGS."
  (utl-run-before-process-hook program args))

;;;###autoload
(defun utl-enable-process-hooks ()
  "Make `utl-before-process-functions' active."
  (interactive)
  (advice-add 'call-process :before #'utl-run-before-call-process-hook)
  (advice-add 'start-process :before #'utl-run-before-start-process-hook))

;;;###autoload
(defun utl-disable-process-hooks ()
  "Make `utl-before-process-functions' inactive."
  (interactive)
  (advice-remove 'call-process #'utl-run-before-call-process-hook)
  (advice-remove 'start-process #'utl-run-before-start-process-hook))

(provide 'utl-process)

;;; utl-process.el ends here
