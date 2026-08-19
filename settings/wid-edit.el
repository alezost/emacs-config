;;; wid-edit.el --- Settings for `wid-edit' package  -*- lexical-binding: t -*-

(require 'wid-edit)
(require 'al-key)

(defconst al/widget-button-keys
  '(("." . widget-backward)
    ("e" . widget-forward)
    ("u" . widget-button-press)
    ;; "m" for "mark"; useful in `recentf-edit-list'.
    ("m"   (widget-button-press (point)) (widget-forward 1)))
  "Alist of auxiliary keys for modes with widget buttons.")
(al/bind-keys-from-vars 'widget-keymap 'al/widget-button-keys t)

(defconst al/widget-field-keys
  '(("<M-tab>" . widget-complete)
    ("M-<" . widget-kill-line)
    ("<ctrl-i>" . widget-end-of-line)
    ("C-k"   (beginning-of-line) (widget-kill-line)))
  "Alist of auxiliary keys for modes with widget fields.")
(al/bind-keys-from-vars 'widget-field-keymap 'al/widget-field-keys)

;; XXX Emacs bug: changing `widget-field-keymap' does nothing because an
;; `editable-field' widget type is already defined by `wid-edit' with
;; the default keymap.  So we need to update the keymap in the widget.
(setf (plist-get (cdr (get 'editable-field 'widget-type))
                 :keymap)
      widget-field-keymap)

;;; wid-edit.el ends here
