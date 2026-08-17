;;; pp.el --- Settings for `pp' package  -*- lexical-binding: t -*-

(require 'pp)
(require 'al-pp)

(advice-add 'pp-display-expression :after #'al/pp-enable-undo)

;;; pp.el ends here
