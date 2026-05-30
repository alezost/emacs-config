;;; dict.el --- Spelling, translating, …  -*- lexical-binding: t -*-

;; Copyright © 2014–2026 Alex Kost

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


;;; Global keys

(al/bind-key "<XF86Spell>" al/translate-tui)

(al/bind-keys
 :prefix-map al/spell-map
 :prefix-docstring "Map for flyspell and friends."
 :prefix "H-s"
 ("r" . flyspell-region)
 ("b" . flyspell-buffer)
 ("n" . flyspell-goto-next-error)
 ("H-n" . flyspell-goto-next-error))


;;; Misc settings and packages

(al/with-eval-after-load ispell
  (ispell-change-dictionary "en" 'global))

(al/setq-no-warnings flyspell-use-meta-tab nil)
(al/with-eval-after-load flyspell
  (defconst al/flyspell-keys
    '(("C-M-g n" . flyspell-goto-next-error))
    "Alist of auxiliary keys for `flyspell-mode-map'.")
  (al/bind-keys-from-vars 'flyspell-mode-map 'al/flyspell-keys))

(al/with-eval-after-load google-translate-core-ui
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

  (al/require al-google-translate))

(al/with-eval-after-load google-translate-smooth-ui
  (google-translate--setup-minibuffer-keymap)
  (defconst al/google-translate-keys
    '(("C-." . google-translate-previous-translation-direction)
      ("C-e" . google-translate-next-translation-direction))
    "Alist of auxiliary keys for `google-translate-minibuffer-keymap'.")
  (al/bind-keys-from-vars 'google-translate-minibuffer-keymap
    '(al/minibuffer-keys al/google-translate-keys))

  (al/add-hook-maybe 'google-translate-mode-hook 'al/text-scale+1))

(al/with-eval-after-load al-google-translate
  (advice-add 'google-translate-listen-translation
    :override #'al/google-translate-listen-translation))

;;; dict.el ends here
