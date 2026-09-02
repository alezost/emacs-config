;;; profiler.el --- Settings for `profiler' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'profiler)

(al/bind-keys
  :map profiler-report-mode-map
  ("↑"     'profiler-report-previous-entry)
  ("↓"     'profiler-report-next-entry)
  ("<tab>" 'profiler-report-toggle-entry)
  ("→"    'profiler-report-toggle-entry))

;;; profiler.el ends here
