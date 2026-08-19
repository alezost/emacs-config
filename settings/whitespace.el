;;; whitespace.el --- Settings for `whitespace' package  -*- lexical-binding: t -*-

(require 'whitespace)

(setq
 whitespace-line-column 78
 whitespace-display-mappings
 `((space-mark   ?\s  [?·])
   (space-mark   ?    [?○])
   ;; (newline-mark ?\n  [?↵ ?\n])
   (newline-mark ?\^L ,(aref standard-display-table ?\^L))
   (tab-mark     ?\t  [?⇉ ?\t]))
 whitespace-style
 '(face spaces tabs trailing lines space-before-tab newline
        indentation space-after-tab tab-mark newline-mark))

;;; whitespace.el ends here
