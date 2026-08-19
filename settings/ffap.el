;;; ffap.el --- Settings for `ffap' package  -*- lexical-binding: t -*-

(require 'al-ffap)

(advice-add 'ffap-read-file-or-url :override #'al/ffap-read-file-or-url)

;;; ffap.el ends here
