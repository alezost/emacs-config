;;; utl-google-translate.el --- Additional functionality for google-translate

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 20 Aug 2013

;;; Code:

(require 'google-translate-default-ui)
(require 'google-translate-smooth-ui)
(require 'utl-misc)


;;; Default UI

(defun utl-%google-translate (override-p reverse-p)
  "Translate region or prompting text.
Alternative to `%google-translate-query-translate' and
`%google-translate-at-point'."
  (let* ((langs (google-translate-read-args override-p reverse-p))
         (source-language (car langs))
         (target-language (cadr langs)))
    (google-translate-translate
     source-language target-language
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (utl-read-string
        (format "Translate from %s to %s: "
                (google-translate-language-display-name source-language)
                (google-translate-language-display-name target-language))
        nil nil (current-word t t))))))

;;;###autoload
(defun utl-google-translate (&optional override-p)
  "Translate region or prompting text from auto detected language.
For the meaning of OVERRIDE-P, see `google-translate-query-translate'."
  (interactive "P")
  (let ((google-translate-default-source-language "auto"))
    (utl-%google-translate override-p nil)))

;;;###autoload
(defun utl-google-translate-direct ()
  "Translate region or prompting text from source to target language.
For details look at `google-translate-query-translate'."
  (interactive)
  (utl-%google-translate nil nil))

;;;###autoload
(defun utl-google-translate-reverse ()
  "Translate region or prompting text from target to source language.
For details look at `google-translate-query-translate'."
  (interactive)
  (utl-%google-translate nil t))


;;; Smooth UI

;;;###autoload
(defun utl-google-translate-smooth-translate ()
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
(defun utl-google-translate-using-languages (source target)
  "Translate a text using SOURCE and TARGET languages."
  (let ((google-translate-translation-directions-alist
         (list (cons source target)
               (cons target source))))
    (utl-google-translate-smooth-translate)))

(provide 'utl-google-translate)

;;; utl-google-translate.el ends here
