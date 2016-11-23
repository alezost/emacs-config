;;; al-comint.el --- Additional functionality for comint

;; Copyright © 2015-2016 Alex Kost

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

(require 'comint)

;;;###autoload
(defun al/comint-previous-matching-input-from-input (arg)
  "Search backwards through input history for match for current input.
Unlike `comint-previous-matching-input-from-input', the matching
input is not forced to begin with the current input."
  (interactive "p")
  (unless (memq last-command '(al/comint-previous-matching-input-from-input
                               al/comint-next-matching-input-from-input))
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
(defun al/comint-next-matching-input-from-input (arg)
  "Search forwards through input history for match for current input."
  (interactive "p")
  (al/comint-previous-matching-input-from-input (- arg)))

(defun al/comint-input-at-point ()
  "Return comint input from the current input (command) line.
Return nil, if the current line is not the input line."
  (let ((beg (field-beginning)))
    (unless (eq (get-char-property beg 'field)
                'output)
      (buffer-substring-no-properties beg (field-end)))))

;;;###autoload
(defun al/comint-send-input-maybe ()
  "Call `comint-send-input' if the point is on the command line."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (let ((prompt (marker-position (process-mark proc))))
        (when (< (point) prompt)
          (let ((input (al/comint-input-at-point)))
            (if (null input)
                (user-error (substitute-command-keys "\
You don't want to do \"\\[al/comint-send-input-maybe]\" here"))
              (goto-char prompt)
              (delete-region prompt (point-max))
              (insert input)))))
      (comint-send-input))))

;;;###autoload
(defun al/comint-toggle-move-point ()
  "Toggle moving point to the end of comint output."
  (interactive)
  (let ((default (default-value 'comint-move-point-for-output)))
    (setq-local comint-move-point-for-output
                (if (eq default comint-move-point-for-output)
                    (not default)
                  default))))

(provide 'al-comint)

;;; al-comint.el ends here
