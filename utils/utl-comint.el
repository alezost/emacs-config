;;; utl-comint.el --- Additional functionality for comint

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 29 Apr 2015

;;; Code:

(require 'comint)

;;;###autoload
(defun utl-comint-previous-matching-input-from-input (arg)
  "Search backwards through input history for match for current input.
Unlike `comint-previous-matching-input-from-input', the matching
input is not forced to begin with the current input."
  (interactive "p")
  (unless (memq last-command '(utl-comint-previous-matching-input-from-input
                               utl-comint-next-matching-input-from-input))
    ;; Starting a new search.
    (setq comint-matching-input-from-input-string
          (buffer-substring
           (or (marker-position comint-accum-marker)
               (process-mark (get-buffer-process (current-buffer))))
           (point))
          comint-input-ring-index nil))
  (comint-previous-matching-input
   (regexp-quote comint-matching-input-from-input-string)
   arg))

;;;###autoload
(defun utl-comint-next-matching-input-from-input (arg)
  "Search forwards through input history for match for current input."
  (interactive "p")
  (utl-comint-previous-matching-input-from-input (- arg)))

(provide 'utl-comint)

;;; utl-comint.el ends here
