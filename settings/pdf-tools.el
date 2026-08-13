;;; pdf-tools.el --- Settings for `pdf-tools' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'pdf-view)
(require 'al-pdf)
(require 'al-key)

(setq-default pdf-view-display-size 'fit-page)

(setq pdf-view-mode-hook
      '(pdf-history-minor-mode
        pdf-isearch-minor-mode
        pdf-links-minor-mode
        pdf-misc-minor-mode
        pdf-outline-minor-mode
        pdf-misc-context-menu-minor-mode
        pdf-cache-prefetch-minor-mode
        pdf-occur-global-minor-mode))

(al/bind-keys
  :map pdf-view-mode-map
  ("h" . al/pdf-view-previous-page)
  ("n" . al/pdf-view-next-page)
  ("c" . pdf-view-themed-minor-mode)
  ([down-mouse-1] . al/pdf-view-select-region)
  ([double-mouse-1] . al/pdf-view-select-word))

(advice-add 'pdf-view-deactivate-region
  :override 'al/pdf-view-deactivate-region)

(al/eval-after-load pdf-outline
  (al/clean-map 'pdf-outline-minor-mode-map)
  (al/bind-keys
   :map pdf-outline-minor-mode-map
   ("i" . pdf-outline))

  (defconst al/pdf-outline-buffer-keys
    '(("TAB" . outline-cycle)
      ("i" . pdf-outline-select-pdf-window)
      ("u" . pdf-outline-follow-link)
      ("d" . pdf-outline-display-link)
      ("q" . quit-window)))
  (al/bind-keys-from-vars 'pdf-outline-buffer-mode-map
    '(al/lazy-moving-keys
      al/lazy-scrolling-keys
      al/pdf-outline-buffer-keys))

  (add-hook 'pdf-outline-buffer-mode-hook #'hl-line-mode))

(al/eval-after-load pdf-links
  (setq pdf-links-convert-pointsize-scale 0.02)

  (al/clean-map 'pdf-links-minor-mode-map)
  (al/bind-keys
   :map pdf-links-minor-mode-map
   ("u" . pdf-links-action-perform)
   ("U" . pdf-links-isearch-link)))

(al/eval-after-load pdf-history
  (al/clean-map 'pdf-history-minor-mode-map)
  (al/bind-keys
   :map pdf-history-minor-mode-map
   ("," . pdf-history-backward)
   ("p" . pdf-history-forward)))

(al/eval-after-load pdf-misc
  (al/clean-map 'pdf-misc-minor-mode-map)
  (al/bind-keys
   :map pdf-misc-minor-mode-map
   ("f" . pdf-misc-display-metadata)
   ("F" . pdf-misc-display-metadata)))

;;; pdf-tools.el ends here
