;;; dict.el --- Spelling, translating, …

;; Copyright © 2014–2016, 2019 Alex Kost

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

(require 'al-key)


;;; Global keys

(al/bind-keys
 :prefix-map al/translation-map
 :prefix-docstring "Map for dictionaries, translating and friends."
 :prefix "<XF86Spell>"
 ("<XF86Spell>" . al/dictem-run-word)
 ;; ("s" . dictem-run-search)
 ;; ("m" . dictem-run-match)
 ("i" . al/dictem-run-show-all-info)
 ("d" . al/dictem-run-dict-search)
 ("q" . dictem-kill-all-buffers)
 ("e"   (al/google-translate-using-languages "en" "ru"))
 ("r"   (al/google-translate-using-languages* :source "ru"
                                              :target '("en" "de" "fr" "la")
                                              :one-way t))
 ("f"   (al/google-translate-using-languages "fr" "ru" "en"))
 ("g"   (al/google-translate-using-languages "de" "ru" "en"))
 ("l"   (al/google-translate-using-languages "la" "ru" "en"))
 ("G" . al/google-translate-smooth-translate))

(al/bind-keys
 :map al/translation-map
 :prefix-map al/translation-multitran-map
 :prefix-docstring "Map for translating with multitran."
 :prefix "m"
 ("e" . web-search-multitran-en/ru)
 ("E" . web-search-multitran-ru/en)
 ("g" . web-search-multitran-de/ru)
 ("G" . web-search-multitran-ru/de))

(al/bind-keys
 :prefix-map al/spell-map
 :prefix-docstring "Map for flyspell and friends."
 :prefix "H-s"
 ("r" . flyspell-region)
 ("b" . flyspell-buffer)
 ("n" . flyspell-goto-next-error)
 ("H-n" . flyspell-goto-next-error))


;;; Misc settings and packages

(with-eval-after-load 'ispell
  (ispell-change-dictionary "en" 'global))

(setq flyspell-use-meta-tab nil)
(with-eval-after-load 'flyspell
  (defconst al/flyspell-keys
    '(("C-M-g n" . flyspell-goto-next-error))
    "Alist of auxiliary keys for `flyspell-mode-map'.")
  (al/bind-keys-from-vars 'flyspell-mode-map 'al/flyspell-keys))

(al/autoload "dictem"
  dictem-run-search
  dictem-run-match)

(with-eval-after-load 'dictem
  (when (require 'al-dictem nil t)
    (dictem-initialize)
    (advice-add 'dictem :override 'al/dictem)
    (advice-add 'dictem-define-on-press
      :override 'al/dictem-define-on-press))
  (setq dictem-use-existing-buffer nil)
  (al/bind-keys
   :map dictem-mode-map
   ("." . dictem-previous-link)
   ("e" . dictem-next-link)
   ("u" . dictem-define-on-press)
   ("h" . dictem-previous-section)
   ("n" . dictem-next-section)
   ("m" . dictem-hyperlinks-menu)
   ("M" . dictem-run-match)
   ("Q" . dictem-kill-all-buffers))
  (add-hook 'dictem-postprocess-match-hook
            'dictem-postprocess-match)
  (add-hook 'dictem-postprocess-definition-hook
            'dictem-postprocess-definition-separator)
  (add-hook 'dictem-postprocess-definition-hook
            'dictem-postprocess-definition-hyperlinks)
  (add-hook 'dictem-postprocess-show-info-hook
            'dictem-postprocess-definition-hyperlinks))

(with-eval-after-load 'al-dictem
  (setq al/dictem-dicts
        '(nil "mueller7" "korolew_en-ru" "korolew_ru-en"
              "slovnyk_ru-en" "ushakov" "fd-eng-lat" "fd-lat-eng")))

(with-eval-after-load 'google-translate-core-ui
  (setq
   google-translate-show-phonetic t
   google-translate-listen-button-label "Listen"))

(with-eval-after-load 'google-translate-smooth-ui
  (when (require 'dvorak-russian-computer nil t)
    (setq google-translate-preferable-input-methods-alist
          '((dvorak-russian-computer "ru"))))
  (setq
   google-translate-input-method-auto-toggling t)

  (google-translate--setup-minibuffer-keymap)
  (defconst al/google-translate-keys
    '(("C-." . google-translate-previous-translation-direction)
      ("C-e" . google-translate-next-translation-direction))
    "Alist of auxiliary keys for google-translate.")
  (al/bind-keys-from-vars 'google-translate-minibuffer-keymap
    '(al/minibuffer-keys al/google-translate-keys)))

;;; dict.el ends here
