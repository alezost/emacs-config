;;; dict.el --- Spelling, translating, …

;; Copyright © 2014-2015 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Global keys

(bind-keys
 :prefix-map al/translation-map
 :prefix "<XF86Spell>"
 ("<XF86Spell>" . utl-dictem-run-word)
 ("s" . dictem-run-search)
 ("m" . dictem-run-match)
 ("i" . utl-dictem-run-show-all-info)
 ("d" . utl-dictem-run-dict-search)
 ("q" . dictem-kill-all-buffers)
 ("e" . (lambda () (interactive)
          (utl-google-translate-using-languages "en" "ru")))
 ("r" . (lambda () (interactive)
          (utl-google-translate-using-languages "ru" "en")))
 ("f" . (lambda () (interactive)
          (utl-google-translate-using-languages "fr" "ru")))
 ("l" . (lambda () (interactive)
          (let ((google-translate-translation-directions-alist
                 '(("la" . "ru") ("ru" . "la")
                   ("la" . "en") ("en" . "la"))))
            (utl-google-translate-smooth-translate))))
 ("g" . (lambda () (interactive)
          (let ((google-translate-translation-directions-alist nil))
            (utl-google-translate-smooth-translate)))))


;;; Misc settings and packages

(use-package ispell
  :defer t
  :config
  (let ((dir (al/guix-user-profile-dir-file "lib/aspell")))
    (when (file-exists-p dir)
      (setq ispell-aspell-data-dir dir
            ispell-aspell-dict-dir dir)))
  (ispell-change-dictionary "en" 'global))

(use-package flyspell
  :defer t
  :diminish "fly"
  :pre-load
  (setq flyspell-use-meta-tab nil)
  :config
  (defconst al/flyspell-keys
    '(("C-M-g n" . flyspell-goto-next-error))
    "Alist of auxiliary keys for `flyspell-mode-map'.")
  (al/bind-keys-from-vars 'flyspell-mode-map 'al/flyspell-keys))

(use-package dictem
  :defer t
  :commands (dictem-run-search dictem-run-match)
  :config
  (when (require 'utl-dictem nil t)
    (dictem-initialize)
    (defalias 'dictem 'utl-dictem)
    (defalias 'dictem-define-on-press 'utl-dictem-define-on-press))
  (setq dictem-use-existing-buffer nil)
  (bind-keys
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

(use-package utl-dictem
  :defer t
  :config
  (setq utl-dictem-dicts
        '(nil "mueller7" "korolew_en-ru" "korolew_ru-en"
              "slovnyk_ru-en" "ushakov" "fd-eng-lat" "fd-lat-eng")))

(use-package google-translate-core-ui
  :defer t
  :config
  (setq google-translate-show-phonetic t))

(use-package google-translate-smooth-ui
  :defer t
  :config
  (when (require 'dvorak-russian-computer nil t)
    (setq google-translate-preferable-input-methods-alist
          '((dvorak-russian-computer "ru"))))
  (setq
   google-translate-translation-directions-alist
   '(("en" . "ru") ("ru" . "en"))
   google-translate-input-method-auto-toggling t)

  (google-translate--setup-minibuffer-keymap)
  (defconst al/google-translate-keys
    '(("C-." . google-translate-previous-translation-direction)
      ("C-e" . google-translate-next-translation-direction))
    "Alist of auxiliary keys for google-translate.")
  (al/bind-keys-from-vars 'google-translate-minibuffer-keymap
    '(al/minibuffer-keys al/google-translate-keys)))

;;; dict.el ends here
