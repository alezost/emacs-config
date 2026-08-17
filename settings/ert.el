;;; ert.el --- Settings for `ert' package  -*- lexical-binding: t -*-

(require 'ert)
(require 'al-key)

(defconst al/ert-results-keys
  '(("RET" . ert-results-describe-test-at-point)
    ("g" . ert-results-rerun-all-tests)
    ("h" . ert-results-previous-test)))

(al/bind-keys-from-vars 'ert-results-mode-map
  '(al/button-keys al/ert-results-keys))

;;; ert.el ends here
