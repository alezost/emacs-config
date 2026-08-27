;;; google-translate.el --- Settings for `google-translate' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'google-translate-core-ui)
(require 'al-general)
(require 'al-key)
(require 'al-google-translate)

(setq
 google-translate-input-method-auto-toggling t
 google-translate-preferable-input-methods-alist
 '((dvorak-russian-computer "ru")
   (korean-hangul "ko"))
 google-translate-show-phonetic t
 google-translate-listen-program "mpv"
 google-translate-listen-button-label "Listen")

(push '("Auto-detect" . "auto")
      google-translate-supported-languages-alist)

(add-hook 'google-translate-mode-hook #'al/text-scale+1)

(advice-add 'google-translate-listen-translation
  :override #'al/google-translate-listen-translation)

(al/eval-after-load google-translate-smooth-ui
  (google-translate--setup-minibuffer-keymap)
  (al/bind-keys
    :map google-translate-minibuffer-keymap
    ("C-↑" 'google-translate-previous-translation-direction)
    ("C-↓" 'google-translate-next-translation-direction)))

;;; google-translate.el ends here
