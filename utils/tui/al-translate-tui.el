;;; al-translate-tui.el --- Transient interface for language translation  -*- lexical-binding: t -*-

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

(require 'seq)
(require 'transient)
(require 'google-translate-core-ui)
(require 'let-macros)
(require 'al-general)
(require 'al-url)
(require 'al-visual)

(defvar al/translate-tui-top-languages
  '("en" "ru" "ko" "ja" "de" "fr" "auto")
  "List of languages that should be on top of `al/translate-tui-languages'.")

(al/defun-lazy al/translate-tui-languages
  "Return list of available languages for minibuffer completion."
  (let* ((name-fun  (lambda (assoc)
                      (concat (cdr assoc) " (" (car assoc) ")")))
         (top-langs nil)
         (langs     (seq-keep
                     (lambda (assoc)
                       (if (member (cdr assoc)
                                   al/translate-tui-top-languages)
                           (progn
                             (push assoc top-langs)
                             nil)
                         (funcall name-fun assoc)))
                     google-translate-supported-languages-alist))
         (top-langs (mapcar (lambda (lang)
                              (funcall name-fun
                                       (rassoc lang top-langs)))
                            al/translate-tui-top-languages)))
    (append top-langs langs)))

(defvar al/translate-tui-text nil
  "Current text to translate.")

(defun al/translate-tui-text ()
  (al/with-face 'font-lock-string-face
    (or al/translate-tui-text "")))

(defun al/translate-tui-set-text (&optional text)
  (setq al/translate-tui-text
        (or text
            (when (region-active-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end)))
            al/translate-tui-text
            (read-string "Text to translate: "))))

(transient-define-suffix al/translate-tui:text ()
  (interactive)
  (al/translate-tui (read-string "Text: " al/translate-tui-text)))

(defun al/translate-tui-read-language (prompt initial-input history)
  ;; `icomplete-mode' uses some rubbish sort.  Avoid it by setting
  ;; `:cycle-sort-function' completion property.
  (let* ((completion-extra-properties '(:cycle-sort-function identity))
         (choice (completing-read prompt (al/translate-tui-languages)
                                  nil nil initial-input history)))
    (and (string-match " (" choice)
         (substring choice 0 (match-beginning 0)))))

(transient-define-argument al/translate-tui:source-language ()
  :description "source language"
  :class 'transient-option
  :key "s"
  :argument "source="
  :reader #'al/translate-tui-read-language
  :always-read t
  :prompt "Source language: ")

(transient-define-argument al/translate-tui:target-language ()
  :description "target language"
  :class 'transient-option
  :key "t"
  :argument "target="
  :reader #'al/translate-tui-read-language
  :always-read t
  :prompt "Target language: ")

(defun al/translate-tui-args ()
  "Return list of arguments for the current `al/translate-tui' transient.
This list has (SOURCE TARGET) form."
  (let* ((args   (transient-args 'al/translate-tui))
         (source (transient-arg-value "source=" args))
         (target (transient-arg-value "target=" args)))
    (list source target)))

(transient-define-suffix al/translate-tui:google-translate (source target)
  "Translate `al/translate-tui-text' from SOURCE to TARGET language
using Google Translate."
  (interactive (al/translate-tui-args))
  (google-translate-translate source target al/translate-tui-text))

(transient-define-suffix al/translate-tui:papago (source target)
  "Translate `al/translate-tui-text' from SOURCE to TARGET language
using Naver Dictionary."
  (interactive (al/translate-tui-args))
  (browse-url (al/url-papago source target al/translate-tui-text)))

(transient-define-suffix al/translate-tui:naver (source target)
  "Translate `al/translate-tui-text' from SOURCE to TARGET language
using Naver Dictionary."
  (interactive (al/translate-tui-args))
  (let* ((langs (list source target))
         (search-fun (if (member "en" langs)
                         'web-search-naver-en
                       'web-search-naver-ru)))
    (funcall search-fun al/translate-tui-text)))

(defvar al/translate-tui-multitran-data
  '((("ru" "en") . web-search-multitran-ru/en)
    (("en" "ru") . web-search-multitran-en/ru)
    (("ru" "de") . web-search-multitran-ru/de)
    (("de" "ru") . web-search-multitran-de/ru))
  "Alist of ((SOURCE TARGET) . FUN) pairs for Multitran.")

(transient-define-suffix al/translate-tui:multitran (source target)
  "Translate `al/translate-tui-text' from SOURCE to TARGET language
using Multitran."
  (interactive (al/translate-tui-args))
  (if-let ((search-fun (alist-get (list source target)
                                  al/translate-tui-multitran-data
                                  nil nil #'equal)))
      (funcall search-fun al/translate-tui-text)
    (user-error "Unknown language pair for Multitran: (%s, %s)"
                source target)))

(defvar al/translate-tui-verbix-data
  '(("en" . web-search-verbix-en)
    ("ko" . web-search-verbix-ko)
    ("de" . web-search-verbix-de)
    ("ja" . web-search-verbix-ja)
    ("fr" . web-search-verbix-fr))
  "Alist of (LANG . FUN) pairs for Verbix.")

(transient-define-suffix al/translate-tui:verbix (source _)
  "Open `al/translate-tui-text' in SOURCE language using Verbix."
  (interactive (al/translate-tui-args))
  (if-let ((search-fun (alist-get source
                                  al/translate-tui-verbix-data
                                  nil nil #'string=)))
      (funcall search-fun al/translate-tui-text)
    (user-error "Unknown language for Verbix: %s" source)))

(defun al/translate-tui-default-value ()
  (if (null al/translate-tui-text)
      (list "source=auto"
            "target=ru")
    (let* ((first-char (seq-first al/translate-tui-text))
           (one-word?  (not (string-match-p "[\n\t ]"
                                            al/translate-tui-text)))
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

;;;###autoload (autoload 'al/translate-tui "al-translate-tui" nil t)
(transient-define-prefix al/translate-tui (&optional text)
  "Interface for language translation."
  :value 'al/translate-tui-default-value
  ["Text"
   (:info #'al/translate-tui-text :format "%d")
   ("T" "set text" al/translate-tui:text)]
  [[("s" "source" al/translate-tui:source-language)]
   [("t" "target" al/translate-tui:target-language)]]
  ["Translate"
   [("g" "Google" al/translate-tui:google-translate)]
   [("p" "Papago" al/translate-tui:papago)
    ("n" "Naver Dictionary" al/translate-tui:naver)]
   [("m" "Multitran" al/translate-tui:multitran)]
   [("v" "Verbix" al/translate-tui:verbix)]]
  (interactive)
  (al/translate-tui-set-text text)
  (transient-setup 'al/translate-tui))

(provide 'al-translate-tui)

;;; al-translate-tui.el ends here
