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

(provide 'al-slime)

;;; al-slime.el ends here
