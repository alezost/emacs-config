;;; icomplete.el --- Settings for `icomplete' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'icomplete)
(require 'al-key)

(setq
 icomplete-scroll t
 icomplete-tidy-shadowed-file-names t
 icomplete-show-matches-on-no-input t)

(al/bind-keys
  :map minibuffer-local-completion-map
  "SPC" "?"
  ("RET" . icomplete-force-complete-and-exit))

(al/bind-keys
  :map minibuffer-local-must-match-map
  ("RET" . icomplete-force-complete-and-exit))

(defconst al/icomplete-keys
  ;; Don't bind "RET" in `icomplete-minibuffer-map' because it has a
  ;; priority over my `al/minibuffer-*-map' keymaps.
  ;; Use `minibuffer-local-completion-map' and
  ;; `minibuffer-local-must-match-map' above.
  '([remap minibuffer-complete-and-exit]
    ([tab] icomplete-force-complete)
    ("C-j" exit-minibuffer)
    ("M-k" al/minibuffer-copy-current-completion)
    ("C-↑" icomplete-backward-completions)
    ("C-↓" icomplete-forward-completions))
  "Alist of auxiliary keys for icomplete maps.")

(defconst al/icomplete-vertical-keys
  '(("H-a" . icomplete-vertical-goto-first)
    ("H-i" . icomplete-vertical-goto-last))
  "Alist of auxiliary keys for `icomplete-vertical-mode-minibuffer-map'.")

(al/bind-keys-from-vars '(icomplete-minibuffer-map
                          icomplete-fido-mode-map)
  'al/icomplete-keys)

(al/bind-keys-from-vars 'icomplete-vertical-mode-minibuffer-map
  'al/icomplete-vertical-keys)

;;; icomplete.el ends here
