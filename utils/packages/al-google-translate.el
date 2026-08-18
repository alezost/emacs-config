;;; al-google-translate.el --- Additional functionality for google-translate  -*- lexical-binding: t -*-

;; Copyright © 2025–2026 Alex Kost

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

(require 'google-translate-core)

(defun al/google-translate-listen-translation (language text)
  "Replacement for `google-translate-listen-translation'."
  (apply #'call-process "mpv" nil nil nil
         (append '("--really-quiet" "--no-config")
                 (google-translate-format-listen-urls text language))))

(provide 'al-google-translate)

;;; al-google-translate.el ends here
