;;; al-read.el --- Minibuffer readers  -*- lexical-binding: t -*-

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

(defun al/read-string (prompt &optional initial-input history
                              default-value inherit-input-method)
  "Similar to `read-string', but put DEFAULT-VALUE in the prompt."
  (let (prompt-beg prompt-end)
    (if (string-match "^\\(.*\\)\\(:\\s-*\\)$" prompt)
        (setq prompt-beg (match-string 1 prompt)
              prompt-end (match-string 2 prompt))
      (setq prompt-beg prompt
            prompt-end ": "))
    (read-string
     (if default-value
         (format "%s (%s)%s" prompt-beg default-value prompt-end)
       (concat prompt-beg prompt-end))
     initial-input history
     default-value inherit-input-method)))

(provide 'al-read)

;;; al-read.el ends here
