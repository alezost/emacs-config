;;; al-sly.el --- Additional functionality for `sly' package  -*- lexical-binding: t -*-

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

(require 'sly)
(require 'sly-autodoc "contrib/sly-autodoc")    ; required by `mrepl'
(require 'sly-mrepl "contrib/sly-mrepl")
(require 'let-macros)

;;;###autoload
(defun al/sly-eval-dwim ()
  "Eval (with SLY) last sexp or region if it is active."
  (interactive)
  (if (use-region-p)
      (sly-eval-region (region-beginning) (region-end))
    (sly-eval-last-expression)))

;;;###autoload
(defun al/sly (&optional other-window)
  "Switch to SLY REPL or start it if needed.
If OTHER-WINDOW is non-nil, show REPL in other window.  Otherwise,
prefer the current window."
  (interactive)
  (if-let ((conn (sly-current-connection))
           (repl (sly-mrepl--find-create conn)))
      (funcall (if other-window
                   'al/display-buffer-other-window
                 'al/display-buffer)
               repl)
    (sly nil nil t)))

(defun al/sly-ports ()
  "Return list of slynk ports to connect using `sly-connect'."
  ;; Ports for Nyxt and StumpWM.
  (delq nil
        (list 4005
              ;; StumpWM port is defined by adding DISPLAY number to 4006.
              ;; See <https://github.com/alezost/shepherd-config/blob/master/init.scm#L559-L564>.
              (when-let ((display (getenv "DISPLAY"))
                         (match (string-match (rx ":" (group (+ digit)))
                                              display))
                         (num (string-to-number (match-string 1 display))))
                (+ 4006 num)))))

;;;###autoload
(defun al/sly-connect (port)
  "Connect to a swank server running locally on PORT."
  (interactive
   (list (string-to-number
          (completing-read "Connect to port: "
                           (mapcar #'number-to-string (al/sly-ports))))))
  (sly-connect sly-lisp-host port))


;;; REPL

(defun al/sly-repl-set-package (package)
  "Set PACKAGE in the current SLY REPL buffer."
  ;; Taken from `sly-mrepl-set-package'.
  (sly-mrepl--eval-for-repl `(slynk-mrepl:guess-and-set-package ,package)))

(defun al/sly-repl-disconnect-or-quit (&optional arg)
  "Close the current connection or kill the inferior process."
  (interactive "P")
  (if arg
      (sly-quit-lisp)
    (sly-disconnect)))

;;;###autoload
(defun al/sly-switch-to-repl-and-enter ()
  "Switch to SLY REPL and enter the current buffer's module.
This command is analogous to `geiser-mode-switch-to-repl-and-enter' for
`scheme-mode' buffers."
  (interactive)
  (let ((pkg (sly-current-package)))
    (al/sly 'other-window)
    (al/sly-repl-set-package pkg)))

(provide 'al-sly)

;;; al-sly.el ends here
