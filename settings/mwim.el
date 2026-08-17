;;; mwim.el --- Settings for `mwim' package  -*- lexical-binding: t -*-

(require 'mwim)

(defun al/mwim-set-default (var fun)
  (set var
       (mapcar (lambda (assoc)
                 (if (eq t (car assoc))
                     (cons t fun)
                   assoc))
               (symbol-value var))))

(al/mwim-set-default 'mwim-beginning-of-line-function
                     'beginning-of-visual-line)
(al/mwim-set-default 'mwim-end-of-line-function
                     'end-of-visual-line)

;;; mwim.el ends here
