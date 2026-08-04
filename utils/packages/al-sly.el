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

(eval-when-compile
  (require 'fp-utils)
  (require 'let-macros))
(require 'cl-lib)
(require 'sly)
(require 'sly-autodoc "contrib/sly-autodoc")    ; required by `mrepl'
(require 'sly-mrepl "contrib/sly-mrepl")

(defun al/sly-current-package ()
  "Return Common Lisp package of the current buffer.
This is a replacement for `sly-current-package'...  Actually, this
function is used only by `al/sly-mode-line-format'.  But since it sets
`sly-buffer-package' variable, `sly-current-package' just returns its
value."
  ;; By default, the current package in REPL depends on the nearest
  ;; prompt which is overkill in my opinion.  Ideally,
  ;; `sly-buffer-package' variable should be set when a package is
  ;; changed for the current prompt.  Anyway, noone needs the current
  ;; package in the mode-line for REPL buffer because you can always see
  ;; it in the REPL prompt.
  (unless (derived-mode-p 'sly-mrepl-mode)
    ;; `sly-buffer-package' variable should be set by
    ;; `sly-current-package' for non-REPL buffers but this is not
    ;; happens for some reason, so by default, a package is searched
    ;; (eventually, by `sly-search-buffer-package') on every mode line
    ;; update (since `sly-current-package' is used in the mode-line
    ;; construct).
    (or sly-buffer-package
        (setq sly-buffer-package
              (sly-current-package)))))

(defun al/sly-change-action-button-label (fun label &rest args)
  "Replace brackets with spaces in LABEL string.
This function is intended to be used as an `around' advice for
`sly-make-action-button'."
  (let* ((label (if (string-match "\\`\\[" label)
                    (concat " " (substring label (match-end 0)))
                  label))
         (label (if (string-match "\\]\\'" label)
                    (concat (substring label 0 (match-beginning 0)) " ")
                  label)))
    (apply fun label args)))

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


;;; Mode line

(defvar al/sly-ml-connection-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] sly-menu)
    map))

(defvar al/sly-ml-pending-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sly-pop-to-events-buffer)
    (define-key map [mode-line mouse-3] 'sly-forget-pending-events)
    map))

(defvar al/sly-ml-dbs-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sly-db-pop-to-debugger)
    map))

(defun al/sly-ml-format-number (n)
  (cond ((and n (< 0 n))
         (format "%d" n))
        (n "-")
        (t "*")))

(defun al/sly-ml-connection (conn-name)
  (propertize conn-name
    'face 'sly-mode-line
    'help-echo "mouse-1: pop-up SLY menu"
    'mouse-face 'mode-line-highlight
    'keymap al/sly-ml-connection-map))

(defun al/sly-ml-pending (conn)
  (let ((pending (length (sly-rex-continuations conn))))
    (apply #'propertize
           (al/sly-ml-format-number pending)
           'help-echo (format "%s %s\n%s\n%s"
                              pending
                              "pending events outgoing"
                              "mouse-1: go to *sly-events* buffer"
                              "mouse-3: forget pending continuations")
           'mouse-face 'mode-line-highlight
           'keymap al/sly-ml-pending-map
           (and (< 0 pending)
                '(face error)))))

(defun al/sly-ml-dbs (conn)
  (let ((dbs (length (sly-db-buffers conn))))
    (apply #'propertize
           (al/sly-ml-format-number dbs)
           'help-echo (format "%s %s\n%s"
                              dbs
                              "SLY-DB buffers waiting"
                              "mouse-1: go to first one")
           'mouse-face 'mode-line-highlight
           'keymap al/sly-ml-dbs-map
           (and (< 0 dbs)
                '(face error)))))

(defun al/sly-mode-line-format ()
  "Simplified and improved version of `sly--mode-line-format'."
  (let* ((conn      (and<= (sly-current-connection)
                           #'process-live-p))
         (conn-name (or (and=> conn #'sly-connection-name)
                        "*"))
         (pkg-name  (and=> (al/sly-current-package)
                           #'sly--pretty-package-name)))
    (cl-list*
     (al/sly-ml-connection conn-name)
     " "
     (and pkg-name
          (list (propertize pkg-name
                  'face 'font-lock-builtin-face)
                " "))
     (if conn
         (list (al/sly-ml-pending conn)
               "|"
               (al/sly-ml-dbs conn))
       (list (propertize "–"
               'help-echo "No connection"))))))

(provide 'al-sly)

;;; al-sly.el ends here
