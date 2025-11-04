;;; al-ffap.el --- Additional functionality for ffap  -*- lexical-binding: t -*-

;; Copyright © 2024 Alex Kost

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

(require 'ffap)

(defun al/ffap-read-file-or-url (prompt guess)
  "Replacement for `ffap-read-file-or-url'.
Do the same as the original function, except return GUESS
if the internal call of `read-file-name' returns \"\".

Intented to be used like so:
  (advice-add \\='ffap-read-file-or-url
              :override #\\='al/ffap-read-file-or-url)"
  (let ((elem (cons ffap-url-regexp #'ffap--url-file-handler)))
    (unwind-protect
        (progn
          (push elem file-name-handler-alist)
          (if (ffap-url-p guess)
              ;; Just return URL at point instead of messing with
              ;; `read-file-name-default'.
              guess
            (unless guess
              (setq guess default-directory))
            (unless (ffap-file-remote-p guess)
              (setq guess (abbreviate-file-name (expand-file-name guess))))
            (let ((val (read-file-name prompt
                                       (file-name-directory guess) nil nil
                                       (file-name-nondirectory guess))))
              ;; `read-file-name-default' (called by `read-file-name')
              ;; returns "" in some cases.
              (if (string= "" val) guess val))))
      (setq file-name-handler-alist (delq elem file-name-handler-alist)))))

(provide 'al-ffap)

;;; al-ffap.el ends here
