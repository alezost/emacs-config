;;; al-ivy.el-- Additional functionality for ivy-mode

;; Copyright © 2017 Alex Kost

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

(require 'ivy)
(require 'al-imenu)

(defvar al/ivy-flx-enabled? (require 'flx nil t))

(defun al/ivy-imenu-sort (name candidates)
  "Re-sort CANDIDATES, an `imenu' index that contains NAME.
Put `al/imenu-eval-after-load-group' and
`al/imenu-sections-group' groups in the beginning."
  ;; The code originates from `ivy-sort-function-buffer'.
  (if (< (length name) 2)
      candidates
    (let* ((candidates (if al/ivy-flx-enabled?
                           (ivy--flx-sort name candidates)
                         candidates))
           (after-load-re (rx-to-string
                           `(and string-start
                                 ,al/imenu-eval-after-load-group)
                           t))
           (section-re (rx-to-string
                        `(and string-start
                              ,al/imenu-sections-group)
                        t))
           after-load-res
           section-res
           rest)
      (dolist (s candidates)
        (cond
         ((string-match-p after-load-re s)
          (push s after-load-res))
         ((string-match-p section-re s)
          (push s section-res))
         (t
          (push s rest))))
      (nconc
       (nreverse after-load-res)
       (nreverse section-res)
       (nreverse rest)))))

(defun al/ivy-add-prompt-count (prompt)
  "Substitution for `ivy-add-prompt-count'.
The problem with that `ivy-add-prompt-count' is that it doesn't
respect \"%\" character, i.e. when PROMPT contains \"%\", it will
not be replaced with \"%%\", and later `ivy--insert-prompt' will
fail trying to `format' a string with unexpected \"%\" signs."
  (if (string-match-p "%.*d" ivy-count-format)
      (concat ivy-count-format
              (if (string-match "%" prompt)
                  (replace-match "%%" t nil prompt)
                prompt))
    prompt))


(defvar al/ivy-format-selected "─► ")
(defvar al/ivy-format-other "   ")

(defun al/ivy-format-function (candidates)
  "Transform CANDIDATES into a string for minibuffer.
This function is suitable for `ivy-format-function'."
  (ivy--format-function-generic
   (lambda (str)
     (concat al/ivy-format-selected
             (propertize str 'face 'ivy-current-match)))
   (lambda (str)
     (concat al/ivy-format-other str))
   candidates
   "\n"))

;;;###autoload
(defun al/ivy-partial ()
  "Complete the current candidate."
  (interactive)
  ;; Remove potential trailing slash.
  (let ((new (if (string-match "\\(.*\\)/\\'" (ivy-state-current ivy-last))
                 (match-string 1 (ivy-state-current ivy-last))
               (ivy-state-current ivy-last))))
    (delete-region (minibuffer-prompt-end) (point-max))
    (insert new)))

;;;###autoload
(defun al/ivy-copy-current-item ()
  "Put the current ivy item into `kill-ring'."
  (interactive)
  (kill-new (ivy-state-current ivy-last)))

(declare-function magit-status "magit" t)

;;;###autoload
(defun al/ivy-magit-status ()
  "Run `magit-status' from the current ivy find file prompt.
It uses the currently specified directory or the current
completion directory if there is some input."
  (interactive)
  (ivy-exit-with-action
   (lambda (_)
     (let ((default-directory
             (if (string= "" ivy-text)
                 ivy--directory
               (expand-file-name (ivy-state-current ivy-last)
                                 ivy--directory))))
       (magit-status)))))

(provide 'al-ivy)

;;; al-ivy.el ends here
