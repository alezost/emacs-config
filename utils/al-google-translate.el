;;; al-google-translate.el --- Additional functionality for google-translate

;; Copyright © 2013–2016, 2019 Alex Kost

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

(require 'cl-lib)
(require 'google-translate-smooth-ui)

;;;###autoload
(defun al/google-translate-smooth-translate ()
  "Translate a text using translation directions.
Similar to `google-translate-smooth-translate', but prompt for
languages (if needed) before text."
  (interactive)
  (setq google-translate-translation-direction-query
        (when (use-region-p)
          (google-translate--strip-string
           (buffer-substring-no-properties
            (region-beginning) (region-end)))))
  (let ((google-translate-translation-directions-alist
         google-translate-translation-directions-alist)
        (google-translate-current-translation-direction
         google-translate-current-translation-direction))
    (unless google-translate-translation-directions-alist
      (let ((source (google-translate-read-source-language))
            (target (google-translate-read-target-language)))
        (setq google-translate-current-translation-direction 0
              google-translate-translation-directions-alist
              (list (cons source target)
                    (cons target source)))))
    (let ((text (google-translate-query-translate-using-directions)))
      (google-translate-translate
       (google-translate--current-direction-source-language)
       (google-translate--current-direction-target-language)
       text))))

;;;###autoload
(defun al/google-translate-using-languages (source &rest targets)
  "Translate a text using SOURCE and TARGETS languages."
  (let ((google-translate-translation-directions-alist
         (cl-mapcan (lambda (target)
                      (list (cons source target)
                            (cons target source)))
                    targets)))
    (al/google-translate-smooth-translate)))

(provide 'al-google-translate)

;;; al-google-translate.el ends here
