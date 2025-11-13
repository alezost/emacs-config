;;; al-transient.el --- Additional functionality for transient library  -*- lexical-binding: t -*-

;; Copyright © 2019 Alex Kost

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

;;; Commentary:

;; I don't like `transient' behavior: it heavily uses minibuffer hooks,
;; pre/post-command hooks, and `overriding-terminal-local-map'.  All
;; this makes impossible to use any key binding when transient is in
;; progress.  You cannot even `edebug' it as it prevents any
;; "non-transient activity".  So this file provides the code to "fix"
;; transient by transforming it into a normal popup buffer with normal
;; major mode and normal keymap.

;; See also <https://github.com/magit/transient/issues/17#issuecomment-493030702>.

;;; Code:

(require 'transient)

(defun al/transient-fix-window ()
  "Return `transient--window' to a 'normal' state."
  (set-window-dedicated-p transient--window nil)
  (set-window-parameter transient--window 'no-other-window nil)
  (with-selected-window transient--window
    (setq
     window-size-fixed nil
     cursor-in-non-selected-windows t
     cursor-type (default-value 'cursor-type)
     mode-line-buffer-identification
     (list ""
           (symbol-name (oref transient--prefix command))
           " " (default-value 'mode-line-buffer-identification)))))

(define-derived-mode al/transient-mode special-mode "al/transient"
  (setq buffer-read-only nil)
  (al/transient-fix-window))

(defun al/transient-push-keymap (map)
  (transient--debug "   al/push %s%s" map
                    (if (symbol-value map) "" " VOID"))
  (with-demoted-errors "ERROR: %S"
    (internal-push-keymap (symbol-value map) 'al/transient-mode-map)))

(defun al/transient-pop-keymap (map)
  (transient--debug "   al/pop %s%s" map
                    (if (symbol-value map) "" " VOID"))
  (with-demoted-errors "ERROR: %S"
    (internal-pop-keymap (symbol-value map) 'al/transient-mode-map)))

(defun al/transient-fix-show (&rest _)
  (transient--debug 'al/transient-fix-show)
  (al/transient-fix-window)
  (select-window transient--window))

(defun al/transient-fix-init (&rest _)
  (transient--debug 'al/transient-fix-init)
  (with-current-buffer transient--buffer-name
    (al/transient-mode)))

(defun al/transient-fix-pre/post-command (fun &rest args)
  (transient--debug 'al/transient-fix-pre/post-command)
  ;; Do anything only for transient commands.
  (when (or (get this-command 'transient--prefix)
            (string-match-p "\\`transient"
                            (symbol-name this-command))
            (and transient--transient-map
                 (string= (buffer-name) transient--buffer-name)
                 (lookup-key transient--transient-map
                             (this-single-command-raw-keys))))
    (apply fun args)))

(defun al/transient-fix-delete-window (fun &rest args)
  (unless (eq transient--exitp 'suspend)
    (apply fun args)))

(provide 'al-transient)

;;; al-transient.el ends here
