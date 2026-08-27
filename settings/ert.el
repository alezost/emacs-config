;;; ert.el --- Settings for `ert' package  -*- lexical-binding: t -*-

(require 'ert)
(require 'al-key)

(al/bind-keys
  :map ert-results-mode-map
  :parent button-buffer-map
  ("RET" 'ert-results-describe-test-at-point)
  ("g" 'ert-results-rerun-all-tests)
  ("h" 'ert-results-previous-test))

;;; ert.el ends here
