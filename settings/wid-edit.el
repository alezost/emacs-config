;;; wid-edit.el --- Settings for `wid-edit' package  -*- lexical-binding: t -*-

(require 'wid-edit)
(require 'al-key)

(al/bind-keys
  :map widget-keymap
  ("↑" 'widget-backward)
  ("↓" 'widget-forward)
  ("→" 'widget-button-press)
  ;; "m" for "mark"; useful in `recentf-edit-list'.
  ("m" (widget-button-press (point)) (widget-forward 1)))

(al/bind-keys
  :map widget-field-keymap
  ("M-<tab>" 'widget-complete)
  ("M-S-↷" 'widget-kill-line)
  ("C-⇥" 'widget-end-of-line)
  ("C-k" (beginning-of-line) (widget-kill-line)))

;; XXX Emacs bug: changing `widget-field-keymap' does nothing because an
;; `editable-field' widget type is already defined by `wid-edit' with
;; the default keymap.  So we need to update the keymap in the widget.
(setf (plist-get (cdr (get 'editable-field 'widget-type))
                 :keymap)
      widget-field-keymap)

;;; wid-edit.el ends here
