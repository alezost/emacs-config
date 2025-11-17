;;; al-google-translate.el --- Additional functionality for google-translate  -*- lexical-binding: t -*-

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

(eval-when-compile (require 'cl-lib))
(require 'google-translate-smooth-ui)
(require 'al-misc)

;;;###autoload
(defun al/google-translate-smooth-translate (&optional languages direction)
  "Translate a text using translation directions.
Similar to `google-translate-smooth-translate', but prompt for
languages (if needed) before text.

LANGUAGES should have a form of `google-translate-translation-directions-alist'.
DIRECTIONS should have a form of `google-translate-current-translation-direction'."
  (interactive)
  (setq google-translate-translation-direction-query
        (when (use-region-p)
          (google-translate--strip-string
           (buffer-substring-no-properties
            (region-beginning) (region-end)))))
  (let ((google-translate-translation-directions-alist languages)
        (google-translate-current-translation-direction (or direction 0)))
    (unless languages
      (let ((source (google-translate-read-source-language))
            (target (google-translate-read-target-language)))
        (setq google-translate-translation-directions-alist
              (list (cons source target)
                    (cons target source)))))
    (let ((text (google-translate-query-translate-using-directions)))
      ;; `google-translate-query-translate-using-directions' ↑ can
      ;; modify source and target languages, so it should be called
      ;; before the following source/target functions.
      (google-translate-translate
       (google-translate--current-direction-source-language)
       (google-translate--current-direction-target-language)
       text))))

;;;###autoload
(cl-defun al/google-translate-using-languages* (&key source target one-way)
  "Translate a text using SOURCE and TARGET languages.
Both, SOURCE and TARGET can be a string or a list of strings with
language names.  If ONE-WAY is non-nil, use only source/target
pairs for translation.  Otherwise, use the reverse
pairs (target/source) as well."
  (let ((languages
         (cl-mapcan
          (lambda (source)
            (cl-mapcan (lambda (target)
                         (if one-way
                             (list (cons source target))
                           (list (cons source target)
                                 (cons target source))))
                       (al/list-maybe target)))
          (al/list-maybe source))))
    (al/google-translate-smooth-translate languages)))

;;;###autoload
(defun al/google-translate-using-languages (source &rest targets)
  "Translate a text using SOURCE and TARGETS languages.
See `al/google-translate-using-languages*' for details."
  (al/google-translate-using-languages* :source source
                                        :target targets))

(provide 'al-google-translate)

;;; al-google-translate.el ends here
