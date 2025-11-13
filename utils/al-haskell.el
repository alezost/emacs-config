;;; al-haskell.el --- Additional functionality for `haskell' mode  -*- lexical-binding: t -*-

;; Copyright © 2022 Alex Kost

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

(require 'haskell)

;;;###autoload
(defun al/haskell-interactive-switch-or-start ()
  "Switch to Haskell interactive buffer or create it if it does not exist."
  (interactive)
  (let ((session (if (cdr haskell-sessions)
                     (haskell-session-choose)
                   (car haskell-sessions))))
    (if session
        (pop-to-buffer (haskell-session-interactive-buffer session))
      (haskell-session-new))))

(provide 'al-haskell)

;;; al-haskell.el ends here
