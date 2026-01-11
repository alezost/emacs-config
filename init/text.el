;;; text.el --- Working with text: editing, searching, …  -*- lexical-binding: t -*-

;; Copyright © 2014–2026 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Global keys for moving

(al/bind-keys
 ("C-o"   . backward-char)
 ("M-o"   . backward-word)
 ("C-M-o" . backward-sexp)
 ("M-O"   . backward-sentence)
 ("C-H-M-o" (scroll-right 1))

 ("C-u"   . forward-char)
 ("M-u"   . forward-word)
 ("C-M-u" . forward-sexp)
 ("M-U"   . forward-sentence)
 ("C-H-M-u" (scroll-left 1))

 ("C-."   . previous-line)
 ("M-."   . backward-paragraph)
 ("C-M-." . backward-up-list)
 ("M->"   . backward-page)
 ("C-H-M-." (scroll-down 1))
 ("H-."   . scroll-down-command)
 ("H-M-." . scroll-other-window-down)
 ("s-."   . al/previous-link)

 ("C-e"   . next-line)
 ("M-e"   . forward-paragraph)
 ("C-M-e" . down-list)
 ("M-E"   . forward-page)
 ("C-H-M-e" (scroll-up 1))
 ("H-e"   . scroll-up-command)
 ("H-M-e" . scroll-other-window)
 ("s-e"   . al/next-link)

 ("C-M-a" . beginning-of-defun)
 ("M-A"   . al/beginning-of-line)
 ("H-a"   . al/beginning-of-buffer)

 ("C-M-i" . end-of-defun)
 ("M-I"   . al/end-of-line)
 ("H-i"   . al/end-of-buffer)

 ("C-3"   . recenter-top-bottom)
 ("C-H-3" . al/recenter-top)
 ("C-2"   . move-to-window-line-top-bottom))

(al/bind-keys
  ("C-a" . beginning-of-line)
  ("<ctrl-i>" . end-of-line))

(unless (display-graphic-p)
  (al/bind-keys
    ("M-."   . previous-line)
    ("M-e"   . next-line)
    ("M-a"   . beginning-of-line)
    ("M-i"   . end-of-line)
    ("M->"   . scroll-down-command)
    ("M-E"   . scroll-up-command)
    ("C-x a" . beginning-of-buffer)
    ("C-x i" . end-of-buffer)
    ("C-M-i" . complete-symbol)))

(al/bind-keys
  :map narrow-map
  ("r" . narrow-to-region))


;;; Global keys for editing

(al/bind-keys
 ("C-,"   . delete-char)
 ("M-,"   . kill-word)
 ("C-M-," . kill-sexp)
 ("M-<"   . kill-line)
 ("H-M-," . al/delete-blank-lines)

 ("C-p"   . delete-backward-char)
 ("M-p"   . backward-kill-word)
 ("C-M-p" . backward-kill-sexp)
 ("M-P"   . al/backward-kill-line)
 ("H-M-p" . delete-trailing-whitespace)

 ("M-q"   . al/fill-paragraph)
 ("C-M-q" . al/save-sexp)
 ("M-Q"   . al/save-line)

 ("C-k"   . kill-whole-line)
 ("M-k"   . al/save-whole-line)
 ("C-M-k" . al/backward-save-sexp)
 ("M-K"   . al/backward-save-line)
 ("H-k"   . al/duplicate-dwim)

 ("C-'"   . transpose-chars)
 ("M-'"     (transpose-words -1))
 ("C-M-'" . transpose-sexps)
 ("M-\""  . transpose-lines)

 ("C-;"   . open-line)
 ("M-;"   . al/comment-dwirm)
 ("C-M-;" . split-line)

 ("C-t"   . kill-region)
 ("M-x"   . kill-ring-save)
 ("C-M-x" . append-next-kill)

 ("S-SPC"     . just-one-space)
 ("M-S-SPC"   . al/delete-horizontal-space)
 ("M-SPC"     . mark-word)
 ("M-s-SPC"   . mark-paragraph)
 ("C-M-s-SPC" . mark-defun)
 ("H-s-SPC"   . mark-whole-buffer)

 ("C-y" . al/yank-or-prev)
 ("M-y" . al/yank-or-next)
 ("C-M-y" . al/insert-clipboard)

 ("C-j" . newline-and-indent)
 ("M-J"   (delete-indentation -1))
 ("<S-backspace>" . delete-region)
 ("H-M-a" . align-regexp)
 ("C-H-M-a" (align-regexp (region-beginning) (region-end)
                          "\\(\\s-*\\)(("))
 ("M-%" . ispell-complete-word)
 ("M-_" . shift-number-down)
 ("M-+" . shift-number-up)

 ("M-/"   . dabbrev-expand)
 ("C-M-/" . hippie-expand)
 ("M-?"   . al/dabbrev-expand-word)

 ("<C-kanji>"   . al/downcase-word-backward)
 ("<S-kanji>"   . al/capitalize-word-backward)
 ("<H-kanji>"   . al/upcase-word-backward)
 ("<C-M-kanji>" . al/downcase-dwim)
 ("<M-S-kanji>" . al/capitalize-dwim)
 ("<H-M-kanji>" . al/upcase-dwim))

(al/bind-keys
 ("<M-tab>" . complete-symbol) ; shadows "\M-\t" ("C-M-i") in any mode, so be careful
 ("<C-tab>" . indent-relative)
 ("<M-S-iso-lefttab>" . tab-to-tab-stop)
 ("<H-tab>" . indent-region)
 ("C-c u" . al/decode-region))


;;; Global keys for inserting text

(define-key key-translation-map [?\C--] [?–])
(define-key key-translation-map [?\C-\M--] [?—])

(when (boundp 'insert-pair-specifications)
  (push '("japanese-quotations" ?「 ?」)
        insert-pair-specifications))

(al/bind-keys
 ("C->"     (insert "->"))
 ("H-4"   . insert-parentheses)
 ("H-M-4" . insert-pair-dollar)
 ("H-5"   . insert-pair-square-brackets)
 ("H-6"   . insert-pair-curly-brackets)
 ("H-,"   . insert-pair-angle-brackets)
 ("H-'"   . insert-pair-single-quotations)
 ("H-;"   . insert-pair-double-quotations)
 ("C-H-," . insert-pair-angle-quotations)
 ("C-H-'" . insert-pair-left-right-single-quotations)
 ("C-H-;" . insert-pair-left-right-double-quotations)
 ("C-H-M-'" . insert-pair-japanese-quotations)
 ("H-`"   . insert-pair-grave-accent-quotation)
 ("C-H-`" . insert-pair-grave-accents))

(al/bind-keys
 :prefix-map al/insert-map
 :prefix-docstring "Map for inserting symbols and text."
 :prefix "M-i"
 ("M-i" . al/insert-delimiter)
 ("M-a"   (insert ";;;###autoload\n"))
 ("t"     (insert "TODO"))
 ("f"     (insert "FIXME"))
 ("d"   . al/insert-date)
 ("M-'" . insert-pair-top-corners)
 ("M-;" . insert-pair-bottom-corners))

(al/autoload "insert-pair" insert-pair-dollar)
(with-eval-after-load 'insert-pair
  (insert-pair-define-command "dollar" ?$ ?$))


;;; Killing and yanking

(setq
 mark-ring-max 30
 set-mark-command-repeat-pop t
 mouse-yank-at-point t
 kill-do-not-save-duplicates t
 mouse-drag-copy-region t
 select-active-regions nil)

(setq select-enable-primary t
      ;; Using clipboard for external programs (like Firefox).
      select-enable-clipboard t)

(al/bind-key "C-H-y" browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (setq
   browse-kill-ring-separator (make-string 64 ?—)
   browse-kill-ring-separator-face nil)
  (defconst al/browse-kill-ring-keys
    '(("."   . browse-kill-ring-previous)
      ("e"   . browse-kill-ring-forward)
      ("u"   . browse-kill-ring-insert-and-quit)
      ("M-d" . browse-kill-ring-edit))
    "Alist of auxiliary keys for `browse-kill-ring-mode-map'.")
  ;; The keys are defined inside `browse-kill-ring-mode'.
  (defun al/browse-kill-ring-bind-keys ()
    (al/bind-keys-from-vars 'browse-kill-ring-mode-map
      'al/browse-kill-ring-keys t))
  (al/add-hook-maybe 'browse-kill-ring-mode-hook
    'al/browse-kill-ring-bind-keys))

(with-eval-after-load 'register
  (setq register-preview-delay 0.3)

  (defun al/insert-register-reverse-arg (fun register &optional arg)
    "Reverse the meaning of ARG for `insert-register'."
    (funcall fun register (not arg)))
  (advice-add 'insert-register
    :around #'al/insert-register-reverse-arg))


;;; Misc settings and packages

(setq-default
 major-mode 'text-mode
 truncate-lines t
 indent-tabs-mode nil
 fill-column 72)

(setq
 save-abbrevs nil
 dabbrev-abbrev-char-regexp "\\sw\\|[-_+*]")

;; Smooth scrolling.
(setq
 mouse-wheel-scroll-amount '(3 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 scroll-conservatively 111
 auto-window-vscroll nil
 next-screen-context-lines 3
 scroll-preserve-screen-position t)

(prefer-coding-system 'utf-8)
(al/modify-syntax text-mode-syntax-table (?\" "\"   "))

(with-eval-after-load 'mwim
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
                       'end-of-visual-line))


;;; Input methods, abbreviations, completions, etc.

(defvar al/input-method-map nil)
(define-prefix-command 'al/input-method-map)
(al/bind-keys
  :map al/input-method-map
  ("<kanji>" . al/set-input-method)
  ("RET"     . al/set-input-method)
  ("C-d"     . describe-input-method)
  ("SPC" (al/set-input-method nil))
  ("e"   (al/set-input-method "emoji"))
  ("6"   (al/set-input-method "al/utf"))
  ("l"   (al/set-input-method "al/utf"))
  ("7"   (al/set-input-method nil))
  ("d"   (al/set-input-method nil))
  ("g"   (al/set-input-method "greek"))
  ("0"   (al/set-input-method "greek"))
  ("j"   (al/set-input-method "japanese"))
  ("h"   (al/set-input-method "japanese-hiragana"))
  ("k"   (al/set-input-method "japanese-katakana"))
  ("u"   (al/set-input-method "korean-hangul"))
  ("c"   (al/set-input-method "korean-hangul"))
  ("t"   (al/set-input-method "TeX"))
  ("8"   (al/set-input-method "dvorak-russian-computer"))
  ("r"   (al/set-input-method "dvorak-russian-computer"))
  ("9"   (al/set-input-method "dvorak-qwerty"))
  ("q"   (al/set-input-method "dvorak-qwerty")))

(defconst al/input-method-keys
  '(("<kanji>" . al/input-method-map)
    ("C-\\" . al/set-input-method)
    ("s-6"   (al/set-input-method "al/utf"))
    ("s-7"   (al/set-input-method nil))
    ("s-8"   (al/set-input-method "dvorak-russian-computer"))
    ("s-9"   (al/set-input-method "dvorak-qwerty"))
    ("s-0"   (al/set-input-method "greek"))
    ("s-M-7" (ispell-change-dictionary "en"))
    ("s-M-8" (ispell-change-dictionary "ru-yeyo")))
  "Alist of auxiliary keys for input methods.")
(al/bind-keys-from-vars nil 'al/input-method-keys)

(with-eval-after-load 'hangul
  (when (require 'al-quail-hangul nil t)
    (advice-add 'hangul2-input-method-internal
      :override #'al/quail-hangul2-input-method-internal)))

(with-eval-after-load 'abbrev
  (define-abbrev-table 'global-abbrev-table
    '(("gos"  "GuixOS")
      ("hhg"  "GNU/Linux")
      ("hhpr" "programming")
      ("hhl"  "LANG=C")
      ("hhy"  "yesterday")
      ("hhY"  "Yesterday")
      ("hh1"  "Monday")
      ("hh2"  "Tuesday")
      ("hh3"  "Wednesday")
      ("hh4"  "Thursday")
      ("hh5"  "Friday")
      ("hh6"  "Saturday")
      ("hh7"  "Sunday"))))

(define-key key-translation-map [?\M-i] 'iso-transl-ctl-x-8-map)
(with-eval-after-load 'iso-transl
  ;; Expand "C-x 8" (now "M-i") map:
  (iso-transl-define-keys
   '(("a"        . [?α])
     ("b"        . [?β])
     ("g"        . [?γ])
     ("l"        . [?λ])
     ("p"        . [?π])
     ("i"        . [?∞])
     ("r"        . [?√])
     ("D"        . [?⌀])
     ("A"        . [?∀])
     ("E"        . [?∃])
     ("nE"       . [?∄])
     ("e"        . [?∈])
     ("ne"       . [?∉])
     ([?\C-0]    . [?∅])
     ("NC"       . [?ℂ])
     ("ND"       . [?𝔻])
     ("NF"       . [?𝔽])
     ("NN"       . [?ℕ])
     ("NP"       . [?ℙ])
     ("NQ"       . [?ℚ])
     ("NR"       . [?ℝ])
     ("NS"       . [?𝕊])
     ("NZ"       . [?ℤ])
     ("s"        . [?☺])
     ("S"        . [?☹])
     ("="        . [?≈])
     (":"        . [?÷])
     ("' "       . [?′])
     ("\" "      . [?″])
     ("/12"      . [?½])
     ("/13"      . [?⅓])
     ("/23"      . [?⅔])
     ("/14"      . [?¼])
     ("/34"      . [?¾])
     ([?\C-.]    . [?·])
     ("<"        . [?≤])
     (">"        . [?≥])
     ("."        . [?…])
     ([?\M-p]    . [?§])
     ([?\M-v]    . [?✓])
     ([?\M-x]    . [?✗])
     ([?\C-u]    . [?↔])
     ([?\C-o]    . [?⇆])
     ([?\M-.]    . [?↑])
     ([?\M-e]    . [?↓])
     ([?\M-o]    . [?←])
     ([?\M-u]    . [?→])
     ([?\C-\M-.] . [?⇑])
     ([?\C-\M-e] . [?⇓])
     ([?\C-\M-o] . [?⇐])
     ([?\C-\M-u] . [?⇒])))

  ;; "M-i <N>" to insert superscript numbers.
  ;; "M-i M-<N>" to insert subscript numbers.
  (defun al/make-number-alist (numbers &optional modifier)
    (seq-map-indexed
     (lambda (char i)
       (cons (kbd (concat modifier (number-to-string i)))
             (vector char)))
     numbers))
  (iso-transl-define-keys
   (append
    (al/make-number-alist "⁰¹²³⁴⁵⁶⁷⁸⁹")
    (al/make-number-alist "₀₁₂₃₄₅₆₇₈₉" "M-"))))

(with-eval-after-load 'pcomplete
  ;; Although `pcomplete-suffix-list' is marked as obsolete, it is used
  ;; by `pcomplete-insert-entry', and its default value prevents
  ;; inserting space after ":" (while completing ERC nicks).
  (setq pcomplete-suffix-list nil)

  (when (require 'al-pcomplete nil t)
    (al/add-hook-maybe '(shell-mode-hook eshell-mode-hook)
      'al/pcomplete-no-space)))

(with-eval-after-load 'pcmpl-args
  (setq
   pcmpl-args-debug-parse-help t
   pcmpl-args-cache-default-duration 999999
   pcmpl-args-cache-max-duration pcmpl-args-cache-default-duration))

(al/bind-key "<C-H-tab>" company-complete)
(al/autoload "company" company-complete)
(with-eval-after-load 'company
  (setq
   company-idle-delay nil
   company-show-numbers t)
  (al/bind-keys
   :map company-active-map
   ("M-." . company-select-previous)
   ("M-e" . company-select-next))
  (global-company-mode))


;;; Searching, finding and replacing

(al/bind-keys
 :map search-map
 ("s"   . query-replace)
 ("M-s" . query-replace)
 ("SPC"   (al/replace " " "_"))
 ("_"     (al/replace "_" " "))
 ("r"   . query-replace-regexp)
 ("R"   . replace-regexp))

(with-eval-after-load 'isearch
  (setq
   isearch-allow-scroll t
   isearch-lax-whitespace nil
   ;; "a" searches for "ä", "à", etc.
   search-default-mode 'char-fold-to-regexp)

  (defconst al/isearch-keys
    '(("M-s" . isearch-query-replace)
      ("M-d" . isearch-edit-string)
      ("M-o" . isearch-occur))
    "Alist of auxiliary keys for `isearch-mode-map'.")
  (al/bind-keys-from-vars 'isearch-mode-map
    '(al/isearch-keys al/input-method-keys)))

(with-eval-after-load 'replace
  (defconst al/occur-keys
    '(("." . occur-prev)
      ("e" . occur-next)
      ("u" . occur-mode-goto-occurrence))
    "Alist of auxiliary keys for `occur-mode-map'.")
  (al/bind-keys-from-vars 'occur-mode-map 'al/occur-keys)

  (defun al/occur-set-paragraph ()
    "Set paragraph to be started from any non-space symbol."
    (setq-local paragraph-start "[^ ]"))
  (al/add-hook-maybe 'occur-mode-hook 'al/occur-set-paragraph))

(with-eval-after-load 'grep
  (setq grep-save-buffers nil))

(with-eval-after-load 'misearch
  (setq multi-isearch-pause nil))

(al/bind-keys
 :prefix-map al/point-pos-map
 :prefix-docstring "Map for point-pos."
 :prefix "M-Z"
 ("M-S" . point-pos-save)
 ("M-D" . point-pos-delete)
 ("M-G" . point-pos-goto)
 ("M-H" . point-pos-previous)
 ("M-N" . point-pos-next)
 ("s" . point-pos-save)
 ("d" . point-pos-delete)
 ("g" . point-pos-goto)
 ("h" . point-pos-previous)
 ("n" . point-pos-next))
(al/bind-keys
 ("C-M-S-g" . point-pos-goto)
 ("C-M-S-h" . point-pos-previous)
 ("C-M-S-n" . point-pos-next))

(al/bind-key* "C-M-s-m" imenu)
(with-eval-after-load 'imenu
  (setq
   ;; imenu-flatten t
   imenu-space-replacement nil
   imenu-level-separator " ⇨ "))

(al/bind-key* "C-M-m" imenus)
(with-eval-after-load 'imenus
  (setq imenus-delimiter imenu-level-separator)

  (defconst al/imenus-keys
    '(("C-r" . imenus-rescan)
      ("C-s" . imenus-exit-to-isearch)
      ("M-s" . imenus-exit-to-occur)))
  (al/bind-keys-from-vars 'imenus-minibuffer-map 'al/imenus-keys))

(al/bind-key "M-s-s" al/imenus-search-elisp-directories)
(with-eval-after-load 'al-imenus
  (setq al/imenus-elisp-directories
        (list al/emacs-init-dir
              al/emacs-utils-dir
              al/emacs-my-packages-dir)))


;;; TeX

(with-eval-after-load 'tex-mode
  (defconst al/tex-keys
    '("C-j")
    "Alist of auxiliary keys for `tex-mode-map'.")
  (al/bind-keys-from-vars 'tex-mode-map 'al/tex-keys))


;;; Working with parentheses (paredit, smartparens)

(al/autoload "paredit"
  paredit-backward-kill-word
  paredit-forward-kill-word
  paredit-backward-up
  paredit-forward-down
  paredit-splice-sexp
  paredit-splice-sexp-killing-backward
  paredit-splice-sexp-killing-forward
  paredit-raise-sexp)

(al/autoload "smartparens"
  sp-indent-defun
  sp-backward-kill-sexp
  sp-kill-sexp
  sp-transpose-sexp
  sp-forward-slurp-sexp
  sp-forward-barf-sexp
  sp-backward-slurp-sexp
  sp-backward-barf-sexp
  sp-cheat-sheet
  sp-absorb-sexp
  sp-emit-sexp
  sp-convolute-sexp
  sp-join-sexp
  sp-split-sexp)

(with-eval-after-load 'smartparens
  (require 'smartparens-config nil t)
  (setq
   sp-navigate-reindent-after-up nil
   sp-ignore-modes-list nil
   sp-wrap-entire-symbol 'globally))

(defvar al/parens-mode-map (make-sparse-keymap))
(al/bind-keys
  :map al/parens-mode-map
  ("<H-M-tab>" . sp-indent-defun)
  ("M-p"       . paredit-backward-kill-word)
  ("M-,"       . paredit-forward-kill-word)
  ("C-M-."     . paredit-backward-up)
  ("C-M-e"     . paredit-forward-down)
  ("H-E"       . paredit-splice-sexp)
  ("H-P"       . paredit-splice-sexp-killing-backward)
  ("H-<"       . paredit-splice-sexp-killing-forward)
  ("H->"       . paredit-raise-sexp)
  ("C-M-p"     . al/backward-kill-sexp)
  ("C-M-,"     . al/kill-sexp)
  ("C-M-'"     . sp-transpose-sexp)
  ("C-)"       . sp-forward-slurp-sexp)
  ("C-M-0"     . sp-forward-barf-sexp)
  ("C-("       . sp-backward-slurp-sexp)
  ("C-M-9"     . sp-backward-barf-sexp))
(al/bind-keys
  :map al/parens-mode-map
  :prefix-map al/parens-misc-map
  :prefix-docstring "Map for misc parens commands."
  :prefix "H-p"
  ("C" . sp-cheat-sheet)
  ("." . sp-absorb-sexp)
  ("e" . sp-emit-sexp)
  ("c" . sp-convolute-sexp)
  ("j" . sp-join-sexp)
  ("s" . sp-split-sexp))

(define-minor-mode al/parens-mode
  "Minor mode for working with parentheses."
  :init-value nil
  :lighter " ()")

(defconst al/parens-ignore-modes
  '(c-mode c++-mode ; because of "<H-M-tab>" (better ideas?)
    nxml-mode)
  "List of modes where `al/parens-mode' should not be enabled.")

(defun al/turn-on-parens-mode ()
  (unless (or (apply #'derived-mode-p al/parens-ignore-modes)
              (boundp 'ido-completing-read)) ; inside ido
    (al/parens-mode)))

(define-globalized-minor-mode al/global-parens-mode
  al/parens-mode al/turn-on-parens-mode)

;;; text.el ends here
