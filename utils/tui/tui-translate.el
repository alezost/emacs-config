;;; tui-translate.el --- Transient interface for language translation  -*- lexical-binding: t -*-

;; Copyright © 2026 Alex Kost

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

(eval-when-compile
  (require 'al-aux-macros)
  (require 'let-macros))
(require 'seq)
(require 'transient)
(require 'google-translate-core-ui)
(require 'al-read)
(require 'al-url)
(require 'al-visual)

(defvar tui/translate-top-languages
  '("en" "ru" "ko" "ja" "de" "fr" "auto")
  "List of languages that should be on top of `tui/translate-languages'.")

(al/defun-lazy tui/translate-languages
  "Return list of available languages for minibuffer completion."
  (let* ((name-fun  (pcase-lambda (`(,name . ,code))
                      (concat code " (" name ")")))
         (top-langs nil)
         (langs     (seq-keep
                     (lambda (assoc)
                       (if (member (cdr assoc)
                                   tui/translate-top-languages)
                           (progn
                             (push assoc top-langs)
                             nil)
                         (funcall name-fun assoc)))
                     google-translate-supported-languages-alist))
         (top-langs (mapcar (lambda (lang)
                              (funcall name-fun
                                       (rassoc lang top-langs)))
                            tui/translate-top-languages)))
    (append top-langs langs)))

(defvar tui/translate-text nil
  "Current text to translate.")

(defun tui/translate-text ()
  (al/with-face 'font-lock-string-face
    (or tui/translate-text "")))

(defun tui/translate-set-text (&optional text)
  (setq tui/translate-text
        (or text
            (when (region-active-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end)))
            tui/translate-text
            (read-string "Text to translate: "))))

(transient-define-suffix tui/translate:text ()
  (interactive)
  (tui/translate (read-string "Text: " tui/translate-text)))

(defun tui/translate-read-language (prompt initial-input history)
  (let ((choice (al/completing-read-no-sort
                 prompt (tui/translate-languages)
                 nil nil initial-input history)))
    (and (string-match " (" choice)
         (substring choice 0 (match-beginning 0)))))

(transient-define-argument tui/translate:source-language ()
  :description "source language"
  :class 'transient-option
  :key "s"
  :argument "source="
  :reader #'tui/translate-read-language
  :always-read t
  :prompt "Source language: ")

(transient-define-argument tui/translate:target-language ()
  :description "target language"
  :class 'transient-option
  :key "t"
  :argument "target="
  :reader #'tui/translate-read-language
  :always-read t
  :prompt "Target language: ")

(defun tui/translate-args ()
  "Return list of arguments for the current `tui/translate' transient.
This list has (SOURCE TARGET) form."
  (let* ((args   (transient-args 'tui/translate))
         (source (transient-arg-value "source=" args))
         (target (transient-arg-value "target=" args)))
    (list source target)))

(transient-define-suffix tui/translate:google-translate (source target)
  "Translate `tui/translate-text' from SOURCE to TARGET language
using Google Translate."
  (interactive (tui/translate-args))
  (google-translate-translate source target tui/translate-text))

(transient-define-suffix tui/translate:papago (source target)
  "Translate `tui/translate-text' from SOURCE to TARGET language
using Naver Dictionary."
  (interactive (tui/translate-args))
  (browse-url (al/url-papago source target tui/translate-text)))

(transient-define-suffix tui/translate:naver (source target)
  "Translate `tui/translate-text' from SOURCE to TARGET language
using Naver Dictionary."
  (interactive (tui/translate-args))
  (let* ((langs (list source target))
         (search-fun (if (member "en" langs)
                         'web-search-naver-en
                       'web-search-naver-ru)))
    (funcall search-fun tui/translate-text)))

(defvar tui/translate-multitran-data
  '((("ru" "en") . web-search-multitran-ru/en)
    (("en" "ru") . web-search-multitran-en/ru)
    (("ru" "de") . web-search-multitran-ru/de)
    (("de" "ru") . web-search-multitran-de/ru))
  "Alist of ((SOURCE TARGET) . FUN) pairs for Multitran.")

(transient-define-suffix tui/translate:multitran (source target)
  "Translate `tui/translate-text' from SOURCE to TARGET language
using Multitran."
  (interactive (tui/translate-args))
  (if-let ((search-fun (alist-get (list source target)
                                  tui/translate-multitran-data
                                  nil nil #'equal)))
      (funcall search-fun tui/translate-text)
    (user-error "Unknown language pair for Multitran: (%s, %s)"
                source target)))

(defvar tui/translate-verbix-data
  '(("en" . web-search-verbix-en)
    ("ko" . web-search-verbix-ko)
    ("de" . web-search-verbix-de)
    ("ja" . web-search-verbix-ja)
    ("fr" . web-search-verbix-fr))
  "Alist of (LANG . FUN) pairs for Verbix.")

(transient-define-suffix tui/translate:verbix (source _)
  "Open `tui/translate-text' in SOURCE language using Verbix."
  (interactive (tui/translate-args))
  (if-let ((search-fun (alist-get source
                                  tui/translate-verbix-data
                                  nil nil #'string=)))
      (funcall search-fun tui/translate-text)
    (user-error "Unknown language for Verbix: %s" source)))

(defun tui/translate-default-value ()
  (if (null tui/translate-text)
      (list "source=auto"
            "target=ru")
    (let* ((first-char (seq-first tui/translate-text))
           (one-word?  (not (string-match-p "[\n\t ]"
                                            tui/translate-text)))
           (script     (aref char-script-table first-char))
           (source     (cond
                        ((eq script 'latin)    "en")
                        ((eq script 'cyrillic) "ru")
                        ((eq script 'hangul)   "ko")
                        ((eq script 'kana)     "ja")
                        (t "auto")))
           (target     (cond
                        ((string= source "ru") "ko")
                        ((and one-word?
                              (not (string= source "en")))
                         "en")
                        (t "ru"))))
      (list (concat "source=" source)
            (concat "target=" target)))))

;;;###autoload (autoload 'tui/translate "tui-translate" nil t)
(transient-define-prefix tui/translate (&optional text)
  "Interface for language translation."
  :value 'tui/translate-default-value
  ["Text"
   (:info #'tui/translate-text :format "%d")
   ("T" "set text" tui/translate:text)]
  [[("s" "source" tui/translate:source-language)]
   [("t" "target" tui/translate:target-language)]]
  ["Translate"
   [("g" "Google" tui/translate:google-translate)]
   [("p" "Papago" tui/translate:papago)
    ("n" "Naver Dictionary" tui/translate:naver)]
   [("m" "Multitran" tui/translate:multitran)]
   [("v" "Verbix" tui/translate:verbix)]]
  (interactive)
  (tui/translate-set-text text)
  (transient-setup 'tui/translate))

(provide 'tui-translate)

;;; tui-translate.el ends here
