;;; ert.el --- Settings for `ert' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'ert)

(al/bind-keys
  :map ert-results-mode-map
  :parent button-buffer-map
  ("RET" 'ert-results-describe-test-at-point)
  ("g" 'ert-results-rerun-all-tests)
  ("h" 'ert-results-previous-test))

;;; ert.el ends here
