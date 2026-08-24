;;; pdf-tools.el --- Settings for `pdf-tools' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'pdf-view)
(require 'al-pdf)
(require 'al-key)

(al/autoload "pdf-links"
  pdf-links-isearch-link)
(al/autoload "pdf-outline"
  pdf-outline-imenu-create-index-tree)
(al/autoload "pdf-misc"
  pdf-misc-display-metadata
  pdf-misc-popup-context-menu)

(declare-function pdf-history-minor-mode "pdf-history")
(declare-function pdf-isearch-minor-mode "pdf-isearch")

(setq-default pdf-view-display-size 'fit-page)

(al/eval-at-hook pdf-view-mode-hook
  (pdf-history-minor-mode)
  (pdf-isearch-minor-mode)
  (pdf-cache-prefetch-minor-mode)
  (setq-local imenu-create-index-function
              'pdf-outline-imenu-create-index-tree))

(al/bind-keys
  :map pdf-view-mode-map
  ("u" . pdf-links-action-perform)
  ("U" . pdf-links-isearch-link)
  ("i" . pdf-outline)
  ("f" . pdf-misc-display-metadata)
  ("h" . al/pdf-view-previous-page)
  ("n" . al/pdf-view-next-page)
  ("c" . pdf-view-themed-minor-mode)
  ([down-mouse-3] . pdf-misc-popup-context-menu)
  ([down-mouse-1] . al/pdf-view-select-region)
  ([double-mouse-1] . al/pdf-view-select-word))

(advice-add 'pdf-view-deactivate-region
  :override 'al/pdf-view-deactivate-region)

(al/eval-after-load pdf-outline
  (al/clean-map 'pdf-outline-minor-mode-map)

  (defconst al/pdf-outline-buffer-keys
    '(([tab] outline-cycle)
      ("i" pdf-outline-select-pdf-window)
      ("u" pdf-outline-follow-link)
      ("d" pdf-outline-display-link)
      ("q" quit-window)))
  (al/bind-keys-from-vars 'pdf-outline-buffer-mode-map
    '(al/lazy-moving-keys
      al/lazy-scrolling-keys
      al/pdf-outline-buffer-keys))

  (add-hook 'pdf-outline-buffer-mode-hook #'hl-line-mode))

(al/eval-after-load pdf-links
  (setq pdf-links-convert-pointsize-scale 0.02)
  (al/clean-map 'pdf-links-minor-mode-map))

(al/eval-after-load pdf-history
  (al/clean-map 'pdf-history-minor-mode-map)
  (al/bind-keys
   :map pdf-history-minor-mode-map
   ("," . pdf-history-backward)
   ("p" . pdf-history-forward)))

;;; pdf-tools.el ends here
