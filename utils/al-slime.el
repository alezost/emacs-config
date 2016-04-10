;;; al-slime.el --- Additional functionality for slime

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 1 Sep 2013

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
