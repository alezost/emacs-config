;;; al-slime.el --- Additional functionality for slime

;; Copyright © 2013-2016 Alex Kost

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

(require 'slime)

;;;###autoload
(defun al/slime-eval-dwim ()
  "Eval (with slime) last sexp or region if it is active."
  (interactive)
  (if (use-region-p)
      (slime-eval-region (region-beginning) (region-end))
    (slime-eval-last-expression)))

(defvar slime-repl-input-start-mark)

;;;###autoload
(defun al/slime-repl-kill-whole-line (arg)
  "Similar to `kill-whole-line', respecting slime-repl prompt."
  (interactive "p")
  (let ((prompt-pos (marker-position slime-repl-input-start-mark)))
    (if (< (point) prompt-pos)
        (kill-whole-line arg)
      (kill-region prompt-pos
                   (progn (forward-line arg) (point))))))

;;;###autoload
(defun al/slime-stumpwm-connect (&optional display)
  "Connect to a swank server running by StumpWM on DISPLAY.
If DISPLAY is nil, use the current DISPLAY environment variable.
With prefix, prompt for DISPLAY.

The port of the running swank server is defined by adding DISPLAY
number to `slime-port'.  See
<https://github.com/alezost/stumpwm-config/blob/master/init.lisp>."
  (interactive
   (list (when current-prefix-arg
           (read-string "Display: " (getenv "DISPLAY")))))
  (let* ((display     (or display (getenv "DISPLAY")))
         (display-num (if (string-match (rx ":" (group (+ digit)))
                                        display)
                          (string-to-number (match-string 1 display))
                        0)))
    (slime-connect slime-lisp-host (+ slime-port display-num))))

(provide 'al-slime)

;;; al-slime.el ends here
