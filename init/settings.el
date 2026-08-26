;;; settings.el --- Miscellaneous settings  -*- lexical-binding: t -*-

;; Copyright © 2012–2026 Alex Kost

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

;;; Code:

(eval-when-compile
  (require 'al-aux-macros)
  (require 'fp-utils))

(require 'al-places)
(require 'al-general)
(require 'al-key)


;;; Key translations

(al/translate-keys ("" "C-" "M-" "C-M-")
  (?. ?↑)
  (?e ?↓)
  (?o ?←)
  (?u ?→)
  (?a ?⇤)
  (?i ?⇥)
  (?, ?↷)
  (?p ?↶))

;; Adding Shift modifiers to the above `al/translate-keys' call does not
;; work because when we press "M-S-e", Emacs thinks that "M-E" is
;; pressed.  We could use `upcase' as a workaround but it will not work
;; for non-letter keys i.e., there is no way to tell that ">" is the
;; same as "S-." (in Dvorak layout).  So we need to treat Shift key
;; translations specially.
(al/translate-keys ((""   "S-")
                    ("M-" "M-S-"))
  (?> ?↑)
  (?E ?↓)
  (?O ?←)
  (?U ?→)
  (?A ?⇤)
  (?I ?⇥)
  (?< ?↷)
  (?P ?↶))

(al/eval-after-frame-init
  :name al/any-frame-key-translations
  ;; Key translation can be done only once for a graphical frame but
  ;; should be performed for any new non-graphical terminal.
  :once nil
  (key-translate "C-x" "C-t")
  (key-translate "C-t" "C-x"))

(al/eval-after-frame-init
  :name al/graphical-frame-key-translations
  :terminal graphical
  :once t
  ;; "C-m" and "RET" are the same key.  One way to distinguish them is
  ;; to use "<return>" instead of "RET" but "RET" is usually bound to
  ;; something sane in various modes (starting from `newline' in
  ;; `global-map') while "<return>" is rarely bound.  Alternative
  ;; approach is to rename "C-m" key as follows, so the default "RET"
  ;; bindings stay untouched and "<ctrl-m>" is free to use.
  (key-translate "C-m" "<ctrl-m>")

  ;; "C-i" and "TAB" are also the same key.  However, I do not do the
  ;; same as above here because I almost always want "TAB" to be bound
  ;; to my `al/tab' command.  So I bind it to "<tab>" and use "C-i" for
  ;; other things.
  ;;
  ;; (key-translate "C-i" "<ctrl-i>")
  )


;;; Keys for multiple maps

(defconst al/free-moving-keys
  '("C-o" "M-o" "C-M-o" "M-O"
    "C-u" "M-u" "C-M-u" "M-U"
    "C-." "M-." "C-M-." "M->"
    "C-e" "M-e" "C-M-e" "M-E"
    "C-a" "M-a" "C-M-a" "M-A"
    "C-i" "M-i" "C-M-i" "M-I")
  "List of moving keys that should be unbound.")

(defconst al/free-editing-keys
  '("C-," "M-," "C-M-," "M-<"
    "C-p" "M-p" "C-M-p" "M-P"
    "M-q" "C-M-q" "M-Q"
    "C-k" "M-k" "C-M-k" "M-K"
    "C-'" "M-'" "C-M-'" "M-\""
    "C-;" "M-;" "C-M-;"
    "C-t" "M-x" "C-M-x")
  "List of editing keys that should be unbound.")

(defconst al/free-important-keys
  '("M-g")
  "List of important keys that should be unbound.")

(defconst al/free-misc-keys
  '("M-_" "M-+"
    "M-/" "C-M-/" "M-?"
    "C-w"
    "<C-tab>" "<M-tab>" "<C-M-tab>")
  "List of miscellaneous keys that should be unbound.")

(defconst al/lazy-moving-keys
  '(("←" backward-char)
    ("→" forward-char)
    ("↑" previous-line)
    ("↓" next-line))
  "Alist of auxiliary keys for lazy moving.")

(defconst al/lazy-scrolling-keys
  '(("SPC" scroll-up-command)
    ("DEL" scroll-down-command))
  "Alist of auxiliary keys for lazy scrolling.")

(defconst al/button-keys
  '(("↑" backward-button)
    ("↓" forward-button)
    ("→" push-button))
  "Alist of auxiliary keys for modes with buttons.")

(setq al/default-keys-variables
      '(al/free-moving-keys
        al/free-editing-keys
        al/free-important-keys))

(al/bind-keys
  :map al/lazy-vertical-moving-map
  :create t
  ("↑" previous-line)
  ("↓" next-line))

(al/bind-keys
  :map al/lazy-moving-map
  :create t
  :parent al/lazy-vertical-moving-map
  ("←" backward-char)
  ("→" forward-char))

(al/bind-keys
  :map al/lazy-scrolling-map
  :create t
  ("SPC" scroll-up-command)
  ("DEL" scroll-down-command))

(al/bind-keys
  :map al/lazy-map
  :create t
  :parent (al/lazy-moving-map
           al/lazy-scrolling-map))


;;; General global keys

(al/bind-keys
  :map ctl-x-map
  ("A"   al/generate-autoloads-from-presets)
  ("C"   save-buffers-kill-emacs)
  ("C-8" insert-char)
  ("o"   al/other-window)
  ("M-o" other-window)
  ("w"   which-key-mode))

(al/bind-keys
  :map ctl-x-map
  :prefix-map al/games-map
  :prefix-doc "Map for games."
  :prefix-key "g"
  ("t" tetris)
  ("d" ducpel)
  ("s" snake))

(al/bind-keys
  :map universal-argument-map
  "C-u"
  ("C-4" universal-argument-more))

(al/bind-keys
  ("C-4"   universal-argument)
  ("H-u"   undo)
  ("H-M-u" undo-only)
  ("C-M-c" calc)

  ("H-m H-m"     kmacro-end-or-call-macro)
  ("<f4>"        kmacro-end-or-call-macro)
  ("<XF86New>"   kmacro-end-or-call-macro)
  ("H-m s"       kmacro-start-macro-or-insert-counter)
  ("H-m RET"     kmacro-start-macro-or-insert-counter)
  ("C-<f4>"      kmacro-start-macro-or-insert-counter)
  ("C-<XF86New>" kmacro-start-macro-or-insert-counter)
  ("H-m e"       kmacro-edit-macro)
  ("H-m C-d"     kmacro-edit-macro)
  ("M-<f4>"      kmacro-edit-macro)
  ("M-<XF86New>" kmacro-edit-macro)
  ("H-m A"       (kmacro-call-macro 0))

  ("C-<kp-add>"      text-scale-increase)
  ("C-<kp-subtract>" text-scale-decrease)
  ("C-<kp-multiply>" (text-scale-set 0))

  ("<f5>"  compile)
  ("C-="   describe-char)
  ("C-c x" exit-recursive-edit)
  ("C-c r" revert-buffer)
  ("C-c k" al/kill-process))

(defalias 'ctl-x-r-prefix ctl-x-r-map)
(al/bind-key "M-R" ctl-x-r-prefix)
(al/bind-keys
  :map ctl-x-r-map
  ("a" append-to-register)
  ("p" prepend-to-register))

(defalias 'goto-prefix goto-map)
(al/bind-key "C-M-g" goto-prefix)
(al/bind-keys
  :map goto-map
  ("C-M-g" goto-line)
  ("c"     move-to-column)
  ("p"     goto-char)
  ("C-M-h" al/previous-error)
  ("C-M-n" al/next-error))

(al/bind-keys
  :prefix-map al/modes-map
  :prefix-doc "Map for enabling/disabling modes."
  :prefix-key "M-M"
  ("M-M" al/major-mode-to-kill-ring)
  ("a" artist-mode)
  ("A" auto-fill-mode)
  ("c" conf-unix-mode)
  ("e" emacs-lisp-mode)
  ("f" font-lock-mode)
  ("o" org-mode)
  ("p" python-mode)
  ("r" rainbow-mode)
  ("l" nlinum-mode)
  ("s" sh-mode)
  ("t" toggle-truncate-lines)
  ("v" view-mode)
  ("T" text-mode))


;;; Text navigating

(al/define-multi-command al/skip-parens-or-forward-word
  parens-skip-forward
  forward-word)

(al/define-multi-command al/skip-parens-or-backward-word
  parens-skip-backward
  backward-word)

(al/bind-keys
  ("C-←"   backward-char)
  ("M-←"   al/skip-parens-or-backward-word)
  ("C-M-←" parens-backward)
  ("M-S-←" backward-sentence)
  ("C-H-M-o" (scroll-right 1))

  ("C-→"   forward-char)
  ("M-→"   al/skip-parens-or-forward-word)
  ("C-M-→" parens-forward)
  ("M-S-→" forward-sentence)
  ("C-H-M-u" (scroll-left 1))

  ("C-↑"   previous-line)
  ("M-↑"   backward-paragraph)
  ("C-M-↑" parens-backward-up)
  ("M-S-↑" backward-page)
  ("C-H-M-." (scroll-down 1))
  ("H-."   scroll-down-command)
  ("H-M-." scroll-other-window-down)
  ("s-."   al/previous-link)

  ("C-↓"   next-line)
  ("M-↓"   forward-paragraph)
  ("C-M-↓" parens-forward-down)
  ("M-S-↓" forward-page)
  ("C-H-M-e" (scroll-up 1))
  ("H-e"   scroll-up-command)
  ("H-M-e" scroll-other-window)
  ("s-e"   al/next-link)

  ("C-⇤"   al/beginning-of-line)
  ("C-M-⇤" beginning-of-defun)
  ("H-a"   al/beginning-of-buffer)
  ("C-x a" beginning-of-buffer)

  ("C-⇥"   al/end-of-line)
  ("C-M-⇥" end-of-defun)
  ("H-i"   al/end-of-buffer)
  ("C-x i" end-of-buffer)

  ("C-3"   recenter-top-bottom)
  ("C-H-3" al/recenter-top)
  ("C-2"   move-to-window-line-top-bottom))

(al/bind-keys
  :map narrow-map
  ("r" narrow-to-region))

(defvar al/last-frame-keys 'graphical
  "Last frame type where some special keys were set.
Used by `al/text-frame-keys' and `al/graphical-frame-keys'.")

(al/eval-after-frame-init
  :name al/text-frame-keys
  :terminal text
  (unless (eq al/last-frame-keys 'text)
    (al/bind-keys
      ;; Some of my main keybindings do not work in a text terminal (in
      ;; virtual terminal), in particular, nothing happens when "C-." or
      ;; "C-," is pressed.  As a workaround, bind "M-" keys to simple
      ;; moving/editing in a text terminal.
      ;;
      ;; Arrow keys work only with "C-" modifier in a text terminal, so
      ;; bind the original (non-translated) keys here.
      ("M-," delete-char)
      ("M-p" delete-backward-char)
      ("M-." previous-line)
      ("M-e" next-line)
      ("M-o" backward-char)
      ("M-u" forward-char)
      ("M-a" al/beginning-of-line)
      ("M-i" al/end-of-line)
      ("M->" scroll-down-command)
      ("M-E" scroll-up-command)
      ("C-M-e" parens-forward-down)
      ("C-M-o" parens-backward)
      ("C-M-u" parens-forward))
    (setq al/last-frame-keys 'text)))

(al/eval-after-frame-init
  :name al/graphical-frame-keys
  :terminal graphical
  (unless (eq al/last-frame-keys 'graphical)
    (al/bind-keys
      ;; No need to restore other "M-" keys because my arrow key
      ;; bindings have a priority over the non-translated ones.
      ("M-i" al/insert-map))
    (setq al/last-frame-keys 'graphical)))

(al/bind-keys
  :prefix-map al/point-pos-map
  :prefix-doc "Map for point-pos."
  :prefix-key "M-Z"
  ("M-S" point-pos-save)
  ("M-D" point-pos-delete)
  ("M-G" point-pos-goto)
  ("M-H" point-pos-previous)
  ("M-N" point-pos-next)
  ("s"   point-pos-save)
  ("d"   point-pos-delete)
  ("g"   point-pos-goto)
  ("h"   point-pos-previous)
  ("n"   point-pos-next))

(al/bind-keys
  ("C-M-S-g" point-pos-goto)
  ("C-M-S-h" point-pos-previous)
  ("C-M-S-n" point-pos-next))

(al/bind-keys*
  ("C-M-s-m" imenu)
  ("C-M-m" imenus)
  ("M-s-s" al/imenus-search-elisp-directories))


;;; Text editing: inserting, deleting, yanking, etc.

(al/bind-keys
  ("C-↷"   delete-char)
  ("M-↷"   parens-kill-word-forward)
  ("C-M-↷" parens-kill-sexp-forward)
  ("M-S-↷" kill-line)
  ("H-M-," al/delete-blank-lines)

  ("C-↶"   delete-backward-char)
  ("M-↶"   parens-kill-word-backward)
  ("C-M-↶" parens-kill-sexp-backward)
  ("M-S-↶" al/backward-kill-line)
  ("H-M-p" delete-trailing-whitespace)

  ("M-q"   al/fill-paragraph)
  ("C-M-q" al/save-sexp)
  ("M-Q"   al/save-line)

  ("C-k"   kill-whole-line)
  ("M-k"   al/save-whole-line)
  ("C-M-k" al/backward-save-sexp)
  ("M-K"   al/backward-save-line)
  ("H-k"   al/duplicate-dwim)

  ("C-'"   transpose-chars)
  ("M-'"   (transpose-words -1))
  ("C-M-'" parens-transpose-sexps)
  ("M-\""  transpose-lines)

  ("C-;"   open-line)
  ("M-;"   al/comment-dwirm)
  ("C-M-;" split-line)

  ("C-t"   al/kill-region)
  ("M-x"   al/kill-ring-save)
  ("C-M-x" append-next-kill)

  ("S-SPC"     just-one-space)
  ("M-S-SPC"   al/delete-horizontal-space)
  ("M-SPC"     mark-word)
  ("M-s-SPC"   mark-paragraph)
  ("C-M-s-SPC" mark-defun)
  ("H-s-SPC"   mark-whole-buffer)

  ("C-y" al/yank-or-prev)
  ("M-y" al/yank-or-next)
  ("H-y" al/insert-clipboard)
  ("C-H-y" browse-kill-ring)

  ("C-j" newline-and-indent)
  ("M-J" (delete-indentation -1))
  ("S-<backspace>" delete-region)
  ("H-M-a" align-regexp)
  ("C-H-M-a" (align-regexp (region-beginning) (region-end)
                           "\\(\\s-*\\)(("))
  ("M-%" ispell-complete-word)
  ("M-_" shift-number-down)
  ("M-+" shift-number-up)

  ("M-/"   dabbrev-expand)
  ("C-M-/" hippie-expand)
  ("M-?"   al/dabbrev-expand-word)

  ("C-<kanji>"   al/downcase-word-backward)
  ("S-<kanji>"   al/capitalize-word-backward)
  ("H-<kanji>"   al/upcase-word-backward)
  ("C-M-<kanji>" al/downcase-dwim)
  ("M-S-<kanji>" al/capitalize-dwim)
  ("H-M-<kanji>" al/upcase-dwim)

  ("C-<tab>" indent-relative)
  ("M-S-<iso-lefttab>" tab-to-tab-stop)
  ("H-<tab>" indent-region)
  ("C-c u" al/decode-region))

(al/bind-keys
  ("C->"   (insert "->"))
  ("H-4"   insert-parentheses)
  ("H-M-4" insert-pair-dollars)
  ("H-5"   insert-pair-square-brackets)
  ("H-6"   insert-pair-curly-brackets)
  ("H-,"   insert-pair-angle-brackets)
  ("H-'"   insert-pair-single-quotations)
  ("H-;"   insert-pair-double-quotations)
  ("C-H-," insert-pair-angle-quotations)
  ("C-H-'" insert-pair-left-right-single-quotations)
  ("C-H-;" insert-pair-left-right-double-quotations)
  ("C-H-M-'" insert-pair-japanese-quotations)
  ("H-`"   insert-pair-grave-accent-quotation)
  ("C-H-`" insert-pair-grave-accents))

(al/bind-keys
  :prefix-map al/insert-map
  :prefix-doc "Map for inserting symbols and text."
  :prefix-key "M-i"
  ("M-i" al/insert-delimiter)
  ("M-a" (insert ";;;###autoload\n"))
  ("t"   (insert "TODO"))
  ("f"   (insert "FIXME"))
  ("d"   al/insert-date)
  ("M-'" insert-pair-top-corners)
  ("M-;" insert-pair-bottom-corners))

(al/bind-keys
  :map key-translation-map
  ("C--"   ?–)
  ("C-M--" ?—)
  ("M-i"   iso-transl-ctl-x-8-map))

;; Expand "C-x 8" (now "M-i") map:
(al/bind-keys
  :map iso-transl-ctl-x-8-map
  :check t
  ("a"     ?α)
  ("b"     ?β)
  ("g"     ?γ)
  ("l"     ?λ)
  ("p"     ?π)
  ("i"     ?∞)
  ("r"     ?√)
  ("D"     ?⌀)
  ("A"     ?∀)
  ("E"     ?∃)
  ("nE"    ?∄)
  ("e"     ?∈)
  ("ne"    ?∉)
  ("C-0"   ?∅)
  ("NC"    ?ℂ)
  ("ND"    ?𝔻)
  ("NF"    ?𝔽)
  ("NN"    ?ℕ)
  ("NP"    ?ℙ)
  ("NQ"    ?ℚ)
  ("NR"    ?ℝ)
  ("NS"    ?𝕊)
  ("NZ"    ?ℤ)
  ("s"     ?☺)
  ("S"     ?☹)
  ("="     ?≈)
  (":"     ?÷)
  ("'"     ?′)
  ("\""    ?″)
  ("/12"   ?½)
  ("/13"   ?⅓)
  ("/23"   ?⅔)
  ("/14"   ?¼)
  ("/34"   ?¾)
  ("<"     ?≤)
  (">"     ?≥)
  ("."     ?…)
  ("C-."   ?·)
  ("M-."   ?↑)
  ("M-e"   ?↓)
  ("M-o"   ?←)
  ("M-u"   ?→)
  ("C-a"   ?⇤)
  ("C-i"   ?⇥)
  ("M-,"   ?↷)
  ("M-p"   ?↶)
  ("C-M-." ?⇑)
  ("C-M-e" ?⇓)
  ("C-M-o" ?⇐)
  ("C-M-u" ?⇒)
  ("<tab>" ?⇉))

;; "M-i <N>" to insert superscript numbers.
;; "M-i M-<N>" to insert subscript numbers.
(al/bind-digits
  :map iso-transl-ctl-x-8-map
  :check t
  (""   "⁰¹²³⁴⁵⁶⁷⁸⁹")
  ("M-" "₀₁₂₃₄₅₆₇₈₉"))

(al/autoload "paredit"
  paredit-splice-sexp
  paredit-splice-sexp-killing-backward
  paredit-splice-sexp-killing-forward
  paredit-raise-sexp)

(al/autoload "smartparens"
  sp-indent-defun
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

(al/eval-after-load smartparens
  (al/require smartparens-config)
  (setq
   sp-navigate-reindent-after-up nil
   sp-ignore-modes-list nil
   sp-wrap-entire-symbol 'globally))

(al/bind-keys
  ("H-M-<tab>" sp-indent-defun)
  ("H-E"   paredit-splice-sexp)
  ("H-P"   paredit-splice-sexp-killing-backward)
  ("H-<"   paredit-splice-sexp-killing-forward)
  ("H->"   paredit-raise-sexp)
  ("C-)"   sp-forward-slurp-sexp)
  ("C-M-0" sp-forward-barf-sexp)
  ("C-("   sp-backward-slurp-sexp)
  ("C-M-9" sp-backward-barf-sexp))

(al/bind-keys
  :prefix-map al/parens-misc-map
  :prefix-doc "Map for miscellaneous parentheses commands."
  :prefix-key "H-p"
  ("C" sp-cheat-sheet)
  ("." sp-absorb-sexp)
  ("e" sp-emit-sexp)
  ("c" sp-convolute-sexp)
  ("j" sp-join-sexp)
  ("s" sp-split-sexp))

(setq
 parens-require-spaces nil
 mark-ring-max 30
 set-mark-command-repeat-pop t
 mouse-yank-at-point t
 kill-do-not-save-duplicates t
 mouse-drag-copy-region t
 select-active-regions nil
 select-enable-primary nil
 select-enable-clipboard nil)

(setq register-preview-delay 0.3)

(defun al/insert-register-reverse-arg (fun register &optional arg)
  "Reverse the meaning of ARG for `insert-register'."
  (funcall fun register (not arg)))

(advice-add 'insert-register
  :around 'al/insert-register-reverse-arg)

(al/eval-settings-after-load
  (browse-kill-ring "browse-kill-ring")
  (dabbrev (setq dabbrev-abbrev-char-regexp "\\sw\\|[-_+*]"))
  (abbrev
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
       ("hh7"  "Sunday")))))


;;; Input methods

;; I rarely need "al/utf" input method for text modes nowadays.
;;
;; (al/call-at-hook after-change-major-mode-hook
;;   al/set-default-input-method)

(al/bind-keys
  :map al/input-method-map
  :create t
  ("<kanji>" al/set-input-method)
  ("RET"     al/set-input-method)
  ("C-d"     describe-input-method)
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
  '(("<kanji>" al/input-method-map)
    ("C-\\"  al/set-input-method)
    ("s-6"   (al/set-input-method "al/utf"))
    ("s-7"   (al/set-input-method nil))
    ("s-8"   (al/set-input-method "dvorak-russian-computer"))
    ("s-9"   (al/set-input-method "dvorak-qwerty"))
    ("s-0"   (al/set-input-method "greek"))
    ("s-M-7" (ispell-change-dictionary "en"))
    ("s-M-8" (ispell-change-dictionary "ru-yeyo")))
  "Alist of auxiliary keys for input methods.")
(al/bind-keys-from-vars nil 'al/input-method-keys)

(al/eval-after-load hangul
  :no-warning t
  (al/require al-quail-hangul))

(al/eval-after-load al-quail-hangul
  (advice-add 'hangul2-input-method-internal
    :override #'al/quail-hangul2-input-method-internal))


;;; Searching, finding and replacing

(al/bind-keys
  :map search-map
  ("s"   query-replace)
  ("M-s" query-replace)
  ("SPC" (al/replace " " "_"))
  ("_"   (al/replace "_" " "))
  ("r"   query-replace-regexp)
  ("R"   replace-regexp))

(al/eval-after-load isearch
  (setq
   isearch-allow-scroll t
   isearch-lax-whitespace nil
   ;; "a" searches for "ä", "à", etc.
   search-default-mode 'char-fold-to-regexp)

  (defconst al/isearch-keys
    '(("M-s" isearch-query-replace)
      ("M-d" isearch-edit-string)
      ("M-o" isearch-occur))
    "Alist of auxiliary keys for `isearch-mode-map'.")
  (al/bind-keys-from-vars 'isearch-mode-map
    '(al/isearch-keys al/input-method-keys)))

(al/eval-after-load replace
  (defconst al/occur-keys
    '(("↑" occur-prev)
      ("↓" occur-next)
      ("→" occur-mode-goto-occurrence))
    "Alist of auxiliary keys for `occur-mode-map'.")
  (al/bind-keys-from-vars 'occur-mode-map 'al/occur-keys)

  (al/eval-at-hook occur-mode-hook
    ;; Start paragraph from any non-space symbol.
    (setq-local paragraph-start "[^ ]")))

(al/eval-settings-after-load
  (grep (setq grep-save-buffers nil
              grep-command "grep -nHi -e "))
  (misearch (setq multi-isearch-pause nil)))


;;; Minibuffer and completions

(al/bind-key* "M-t" execute-extended-command)

(setq
 completion-show-help nil
 completion-auto-select 'second-tab
 completion-styles '(basic substring partial-completion emacs22)
 completions-sort 'historical
 completions-format 'one-column
 completions-header-format (propertize "%s completions:\n" 'face 'alect-title)
 completions-max-height 20
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t
 enable-recursive-minibuffers t)

(al/bind-keys
  :map minibuffer-local-map
  ([tab] completion-at-point)
  ("M-↑" previous-history-element)
  ("M-↓" next-history-element))

(al/bind-keys
  :map read--expression-map
  :check t
  ("C-j" newline))

(al/call-at-hook minibuffer-setup-hook al/hbar-cursor-type)

;; (al/call-after-init icomplete-vertical-mode)
;;
;; Enabling `icomplete-vertical-mode' manually to avoid loading
;; `icomplete' on Emacs start and load it only when minibuffer is used
;; for the first time.
(al/autoload "icomplete"
  icomplete-minibuffer-setup
  icomplete--vertical-minibuffer-setup)
(al/eval-after-init
  (setq icomplete-mode t
        icomplete-vertical-mode t)
  (al/call-at-hook minibuffer-setup-hook
    icomplete-minibuffer-setup)
  (al/call-at-hook icomplete-minibuffer-setup-hook
    icomplete--vertical-minibuffer-setup))

(al/bind-keys
  :map completion-list-mode-map
  ("↑" previous-completion)
  ("↓" next-completion))

(al/eval-after-load al-complete
  :load after-init
  (setq
   completion-styles '(al/split)
   completion-ignored-extensions
   '("./" "../"
     ".o" ".bin" ".lbin" ".so" ".a" ".la" ".lo"
     ".elc" ".go" ".pyc"))

  ;; This breaks company completions in SLY buffers
  ;; (advice-add 'completion--styles :override #'al/completion-styles)

  (advice-add 'completion-all-completions :around #'al/completion-all-completions))

(al/eval-after-load al-minibuffer
  :load after-init
  (al/bind-keys
    :map al/minibuffer-buffer-map
    ("M-m" al/minibuffer-magit-buffers)
    ("M-s" al/minibuffer-shell-buffers))
  (al/bind-keys
    :map al/minibuffer-file-map
    ("M-h" (al/minibuffer-set-directory "~"))
    ("M-m" al/minibuffer-enter-magit-status)
    ("RET" icomplete-fido-ret)
    ("DEL" icomplete-fido-backward-updir))
  (al/bind-keys
    :map al/minibuffer-symbol-map
    ("C-d" al/minibuffer-describe-symbol)
    ("M-d" al/minibuffer-find-symbol))

  (advice-add 'read-file-name             :around #'al/read-file-add-keymap)
  (advice-add 'read-buffer                :around #'al/read-buffer-add-keymap)
  (advice-add 'read-extended-command      :around #'al/read-symbol-add-keymap)
  (advice-add 'read-face-name             :around #'al/read-symbol-add-keymap)
  (advice-add 'help-fns--describe-function-or-command-prompt ; used by `describe-function'
    :around #'al/read-symbol-add-keymap)

  (advice-add 'find-file                :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'switch-to-buffer         :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'execute-extended-command :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'describe-function        :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'describe-variable        :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'describe-face            :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'describe-symbol          :around #'al/minibuffer-fallback-or-funcall))

(al/define-multi-command al/tab
  al/tempo-complete-maybe
  al/indent-maybe
  company-complete)

(al/bind-keys
  ([tab]     al/tab)
  ([backtab] completion-at-point)
  ([M-tab]   al/complete-elisp-symbol))

(al/eval-settings-after-load
  (icomplete "icomplete")
  (pcomplete "pcomplete")
  (pcmpl-args
   (setq
    pcmpl-args-debug-parse-help t
    pcmpl-args-cache-default-duration 999999
    pcmpl-args-cache-max-duration pcmpl-args-cache-default-duration))
  (company "company"))


;;; Working with buffers: ibuffer, uniquify, …

(al/eval-after-load al-buffer-cmd
  (al/bind-keys
    :map al/switch-buffer-map
    ("M-b" al/switch-to-other-buffer)
    ("M-N" al/switch-to-next-buffer)
    ("M-B" al/switch-to-prev-buffer)))

(al/bind-keys*
  ("M-b" al/switch-to-previous-buffer)
  ("C-M-b" (al/find-file (al/notes-dir-file "bookmarks.org"))))

(al/bind-keys*
  :prefix-map al/buffer-map
  :prefix-doc "Map for managing/switching to buffers."
  :prefix-key "C-b"
  ("C-b" switch-to-buffer)
  ("M-b" ibuffer)
  ("r" rename-buffer)
  ("c" clone-buffer)
  ("n" info)
  ("b" al/buffer-name-to-kill-ring)
  ("f" al/file-name-to-kill-ring)
  ("g" (al/display-buffer "*grep*"))
  ("o" (al/display-buffer "*Occur*"))
  ("h" (al/display-buffer "*Help*"))
  ("s" al/switch-to-scratch)
  ("P" list-processes)
  ("E" list-environment)
  ("e" emoji-list)
  ("F" al/switch-to-faces)
  ("w" al/switch-to-w3m)
  ("m" man)
  ("M" woman)
  ("k" (kill-buffer nil))
  ("8" al/switch-to-characters))

(al/eval-settings-after-load
  (uniquify (setq uniquify-buffer-name-style 'post-forward))
  (ibuffer "ibuffer"))


;;; Working with windows and frames

(setq split-width-threshold 120)

(al/call-at-hook window-configuration-change-hook
  al/set-windows-num-property)

(defvar al/display-buffer-regexp
  (rx (or "*Apropos"
          "*Character List*"
          "*Character Set List*"
          "*Colors*"
          "*Diff*"
          "*Faces*"
          "*Google Translate*"
          "*Help*"
          "*Messages*"
          "*Occur*"
          "*Personal Keybindings*"
          "*Proced*"
          "*Process"
          "*Shadows*"
          "*magit:"))
  "Regexp for buffers that should be displayed specially.")

(setq display-buffer-alist
      `(;; Open some buffers in the same window.
        (,al/display-buffer-regexp
         (display-buffer-reuse-window
          display-buffer-same-window))))

(al/bind-keys
  ("H-<XF86AudioRaiseVolume>"   (enlarge-window 1 t))
  ("H-<XF86AudioLowerVolume>"   (enlarge-window -1 t))
  ("M-H-<XF86AudioRaiseVolume>" (enlarge-window 1))
  ("M-H-<XF86AudioLowerVolume>" (enlarge-window -1))
  ("H-o"   al/other-window)
  ("H-M-o" al/switch-or-next-window)
  ("H-M-q" (quit-window nil (previous-window)))
  ("H-O"   al/switch-to-minibuffer)
  ("H-1"   delete-other-windows)
  ("H-2"   al/make-vertical-windows)
  ("H-3"   al/make-horizontal-windows))


;;; Working with files: backup, autosave, dired, etc.

(al/bind-key* "M-C-f" find-file-at-point)
(al/bind-key "H-j" dired-jump)

(al/bind-keys*
  :prefix-map al/find-file-map
  :prefix-doc "Map for finding files."
  :prefix-key "C-f"
  ("C-f"   find-file)
  ("p"     al/find-file-in-path)
  ("S"     al/sudo-find-file)
  ("h"     al/ssh-find-file)
  ("z"     al/router-get-log)
  ("u"     al/browse-url)
  ("y"     al/browse-youtube-video)
  ("l"     find-library)
  ("r"     recentf-open)
  ("e"     (al/find-file al/emacs-dir))
  ("C-c"   (al/find-file al/emacs-settings-dir))
  ("i"     (al/find-file (al/emacs-init-dir-file "init.el")))
  ("s"     (al/find-file (al/emacs-init-dir-file "settings.el")))
  ("c"     (al/find-file (al/emacs-my-packages-dir-file "alect-themes")))
  ("C-M-c" (al/find-file (al/emacs-my-packages-dir-file
                          "alect-themes/alect-themes.el"))))

(al/bind-keys
  :prefix-map al/bookmark-map
  :prefix-doc "Map for bookmarks and finding files."
  :prefix-key "M-f"
  ("M-f"   bookmark-jump)
  ("n"     bookmark-set)
  ("k"     bookmark-delete)
  ("l"     bookmark-bmenu-list)
  ("q"     (al/find-file (al/src-dir-file "emacs/melpa/recipes")))
  ("h"     (al/find-file "~"))
  ("d"     (al/find-file al/journal-dir))
  ("w"     (al/find-file al/download-dir))
  ("M-d"   (find-file al/download-dir))
  ("M-n"   (al/find-file al/notes-dir))
  ("t"     (al/find-file al/tmp-dir))
  ("m"     (al/find-file al/music-dir))
  ("p"     (al/find-file al/progs-dir))
  ("b"     (al/find-file (al/config-dir-file "shell")))
  ("g"     (al/find-file (al/config-dir-file "guile")))
  ("M-c"   (al/find-file al/config-dir))
  ("C-M-c" (find-file (al/config-dir-file "config.scm")))
  ("M-g"   (al/find-file al/guix-profile-dir))
  ("s"     (al/find-file (al/config-dir-file "stumpwm")))
  ("v"     (al/find-file "/var/log")))

(al/bind-keys
  :prefix-map al/grep-find-map
  :prefix-doc "Map for find/grep commands."
  :prefix-key "M-F"
  ("g" grep)
  ("n" find-name-dired)
  ("a" find-dired)
  ("f" grep-find))

(al/bind-keys
  :map ctl-x-map
  :prefix-map al/recentf-map
  :prefix-doc "Map for recent files."
  :prefix-key "r"
  ("m" recentf-mode)
  ("f" recentf-open)
  ("l" recentf-edit-list)
  ("c" recentf-cleanup))

(let ((dir (al/emacs-data-dir-file "auto-save")))
  ;; Emacs does not create a directory of an autosave file and just
  ;; complains when it doesn't exist.
  (unless (file-exists-p dir)
    (al/with-demoted-errors "Making auto-save directory failed: %S"
      (mkdir dir t)))
  (setq auto-save-file-name-transforms
        `((".*" ,(file-name-as-directory dir) t))))

(setq
 auto-save-list-file-prefix
 (al/emacs-data-dir-file "auto-save-list/.saves-")
 backup-directory-alist
 `(;;(,tramp-file-name-regexp . nil)
   (".*" . ,(al/emacs-data-dir-file "backup")))
 backup-by-copying t            ; overwrite backups, not original files
 version-control t
 kept-old-versions 2
 kept-new-versions 4
 delete-old-versions t)

;; Set it before loading `dired' to avoid extra process call (executed
;; by `dired-guess-shell-gnutar' defcustom).
(al/setq-no-warnings dired-guess-shell-gnutar "tar")

(al/eval-after-load al-backup
  :load after-init
  (setq
   al/backup-ignored-regexps
   '("gnus/mail/archive/sent"
     "COMMIT_EDITMSG")
   backup-enable-predicate #'al/backup-enable-predicate)
  (advice-add 'make-backup-file-name-1
    :override #'al/make-backup-file-name-1))

(al/eval-after-load recentf
  (setq
   recentf-exclude (list (al/file-regexp "el" "gz")
                         #'file-remote-p)
   recentf-keep (list #'file-exists-p)
   recentf-used-hooks nil
   recentf-auto-cleanup 'never
   recentf-max-saved-items 300
   recentf-save-file (al/emacs-data-dir-file "recentf")))

(al/eval-after-load saveplace
  (setq
   ;; For some reason, `save-place-loaded' is t after `saveplace' load.
   ;; This bug(?) appeared somewhere between Emacs 29.4 and Emacs 30.1.
   ;; Set this variable back to nil.  Otherwise, `save-place-alist' is
   ;; empty because `save-place-file' is never loaded.
   save-place-loaded nil
   save-place-ignore-files-regexp
   (rx-to-string `(or (and string-start "/gnu")
                      (regexp ,save-place-ignore-files-regexp))
                 'no-group)
   save-place-forget-unreadable-files nil
   save-place-file (al/emacs-data-dir-file "save-places")
   save-place-limit 999)

  (al/require al-saveplace))

(al/eval-after-load al-saveplace
  (advice-add 'save-places-to-alist
    :override #'al/save-places-to-alist))

(al/eval-settings-after-load
  (dired "dired")
  (ffap "ffap")
  (bookmark "bookmark")
  (mailcap
   ;; Use "sxiv" instead of "display" to open image files.  Actually,
   ;; (mailcap-add "image/.*" "sxiv %s") can be used, but it adds the
   ;; entry to the beginning of "image" alist, while I want to fallback
   ;; to "sxiv" as it is done with "display".
   (let* ((image-alist   (cdr (assoc "image" mailcap-mime-data)))
          (display-alist (cdr (assoc ".*" image-alist))))
     (setcdr (assq 'viewer display-alist) "sxiv %s")))
  (al-file-cmd
   (setq
    al/ssh-default-user (list user-login-name "root" "lena")
    al/ssh-default-host "hyperion")))


;;; Version control

(al/bind-keys
  :prefix-map al/magit-map
  :prefix-doc "Map for magit and git stuff."
  :prefix-key "M-m"
  ("M-m" al/magit-switch-buffer)
  ("b" (al/magit-switch-buffer 'all))
  ("B" magit-blame)
  ("c" al/magit-show-commit)
  ("d" magit-dispatch)
  ("s" magit-status)
  ("l" magit-log-current)
  ("k" al/browse-at-remote-kill)
  ("u" browse-at-remote))

;; I don't load "magit-autoloads.el", so autoload some commands.
(al/autoload "magit"
  magit-dispatch)
(al/autoload "magit-status"
  magit-status)
(al/autoload "magit-blame"
  magit-blame)
(al/autoload "magit-log"
  magit-log-current)

(al/setq-no-warnings
 magit-auto-revert-mode nil
 magit-define-global-key-bindings nil

 ;; By default, when `with-editor' library is loaded, it runs
 ;; "<emacsclient> --version" shell command HUNDREDS of times (for any
 ;; possible name of <emacsclient> executable in all dirs from PATH).
 ;; This happens during initializing `with-editor-emacsclient-executable'
 ;; variable (when `with-editor-locate-emacsclient' is called).
 with-editor-emacsclient-executable
 (expand-file-name "emacsclient" invocation-directory)

 ;; `magit-log-margin' should be set before magit is loaded, as
 ;; the other margins are defined from this one.
 magit-log-margin '(t age-abbreviated magit-log-margin-width t 20))

(al/eval-settings-after-load
  (vc-hooks (setq vc-make-backup-files t
                  vc-handled-backends nil))
  (magit "magit")
  (magit-popup "magit-popup"))


;;; Programming modes

(al/bind-key "C-c d" toggle-debug-on-error)

(al/bind-key* "M-v" al/pp-eval-expression)

(al/bind-keys
  ("C-v"   al/eval-dwim)
  ("C-s-v" al/pp-eval-dwim)
  ("C-S-v" al/pp-macroexpand-last-sexp)
  ("C-M-v" eval-defun)
  ("M-s-v" eval-buffer)
  ("C-d"   elisp-slime-nav-describe-elisp-thing-at-point)
  ("M-d"   elisp-slime-nav-find-elisp-thing-at-point))

(al/bind-keys
  :prefix-map al/doc-map
  :prefix-doc "Map for documentation/finding definitions."
  :prefix-key "C-M-d"
  ("f" find-function)
  ("v" find-variable)
  ("F" find-face-definition)
  ("b" describe-personal-keybindings))

;; `al/sly-keys' and `al/geiser-keys' are used by
;; `al/erc-channel-config'.
(defconst al/sly-keys
  '(("C-c"   sly-prefix-map)
    ("C-v"   al/sly-eval-dwim)
    ("C-M-v" sly-eval-defun)
    ("M-s-v" sly-eval-buffer)
    ("C-S-v" sly-macroexpand-all)
    ("C-d"   sly-describe-symbol)
    ("M-d"   sly-edit-definition)
    ("C-M-d" sly-doc-map)))

(defconst al/geiser-keys
  '(("C-v"   al/geiser-eval-dwim)
    ("C-S-v" geiser-expand-last-sexp)
    ("C-M-v" geiser-eval-definition)
    ("M-s-v" geiser-eval-buffer)
    ("C-d"   geiser-doc-symbol-at-point)
    ("M-d"   geiser-edit-symbol-at-point)
    ("C-M-d" al/geiser-doc-map)
    ("C-c l" al/geiser-add-to-load-path)
    ("C-c a" geiser-autodoc-mode)
    ("C-c j" switch-to-geiser-module)
    ;; Although this "C-c C-z" exists in `geiser-mode-map',
    ;; `al/geiser-keys' is also used in ERC buffers.
    ("C-c C-z" geiser-mode-switch-to-repl)
    ("C-c C-j" geiser-mode-switch-to-repl-and-enter)))

(al/autoload "python" python-shell-switch-to-shell)

(setq eval-expression-print-length nil)

(al/setq-no-warnings gud-key-prefix (key-parse "M-G"))

(al/eval-after-load prog-mode
  (al/bind-keys
    :map prog-mode-map
    :clean t
    ("C-M-<tab>" prog-indent-sexp))

  (al/call-at-hook prog-mode-hook
    hl-line-mode
    hl-todo-mode
    abbrev-mode
    al/set-comment-column
    al/show-trailing-whitespace))

(al/eval-after-load lisp-mode
  (al/bind-keys
    :map lisp-mode-shared-map
    :clean t
    ("C-M-<tab>" al/indent-sexp)
    ("C-c C-z" al/ielm-other-window))
  (al/clean-keymap lisp-mode-map)

  (al/modify-page-break-syntax lisp-mode-syntax-table)

  ;; `lisp-mode' package is already loaded on Emacs start, and I don't
  ;; want to load additional Common Lisp functionality on start.  So
  ;; instead of requiring `al-clisp' here, it is loaded on the first run
  ;; of `lisp-mode' major mode (by `lisp-mode-hook').
  (al/eval-at-hook lisp-mode-hook
    :once t
    (al/require al-clisp)
    ;; Update fontification of the current buffer.
    (lisp-mode)))

(al/eval-after-load al-clisp
  (al/clisp-add-font-lock-keywords))

(al/eval-after-load elisp-mode
  (al/clean-keymap emacs-lisp-mode-map)

  ;; `elisp--form-quoted-p' is used only by `elisp-completion-at-point'
  ;; to define if all types of symbols should be completed or only
  ;; variables.  I always want to complete all symbols!
  (advice-add 'elisp--form-quoted-p :override #'always)

  ;; See comment for `lisp-mode-hook' above.
  (al/eval-at-hook emacs-lisp-mode-hook
    :once t
    (al/require al-elisp)
    (emacs-lisp-mode)))

(al/eval-after-load al-elisp
  (al/elisp-add-font-lock-keywords))

(al/eval-settings-after-load
  (ielm "ielm")
  (eldoc (setq eldoc-idle-delay 0.3))
  (pp "pp")
  (ert "ert")
  (debug "debug")
  (edebug "edebug")
  (sly "sly")
  (scheme "scheme")
  (geiser-mode "geiser")
  (haskell-mode "haskell-mode")
  (python "python")
  (js "js")
  (cc-mode "cc-mode")
  (make-mode "make-mode")
  (compile "compile")
  (gud "gud"))


;;; comint, shell, eshell

(setq shell-file-name "bash")

(al/bind-keys
  ("s-s"   al/shell)
  ("C-z"   al/eshell)
  ("C-M-z" al/eshell-cd))

(al/bind-keys*
 :prefix-map al/repl-map
 :prefix-doc "Map for various REPLs."
 :prefix-key "C-n"
 ("C-s" al/switch-to-shell-buffer)
 ("t"   visit-ansi-term)
 ("e"   eshell)
 ("i"   ielm)
 ("a"   al/agent-shell)
 ("s"   al/sql-switch-or-connect)
 ("l"   al/sly)
 ("L"   al/sly-connect)
 ("g"   al/geiser-guile-switch-current-window)
 ("G"   al/geiser-socket-connect)
 ("h"   al/haskell-interactive-switch-or-start)
 ("P"   run-python)
 ("p"   python-shell-switch-to-shell)
 ("m"   maxima)
 ("x"   guix-switch-to-repl))

(al/eval-settings-after-load
  (comint "comint")
  (shell "shell")
  (esh-mode "eshell")
  (agent-shell "agent-shell"))


;;; Button, custom, widget

(al/autoload "bui-button" bui-button-copy-label)

(al/eval-after-load button
  (al/bind-keys
    :map button-buffer-map
    ("↑" backward-button)
    ("↓" forward-button))
  (al/bind-keys
    :map button-map
    ("→" push-button)
    ("c" bui-button-copy-label)))

(al/eval-settings-after-load
  (wid-edit "wid-edit")
  (cus-edit "cus-edit"))


;;; Help, apropos, man, info

(setq help-window-keep-selected t)

(al/bind-keys
  :map help-map
  ("v" al/describe-variable)
  ("s" al/describe-symbol)
  ("x" describe-syntax)
  ("F" describe-face)
  ("K" describe-keymap)
  ("A" apropos))

(al/bind-keys
  :map help-map
  :prefix-map al/info-map
  :prefix-doc "Map to display info manuals."
  :prefix-key "i"
  ("i" (info "dir"))
  ("c" (info "cl"))
  ("e" (info "elisp"))
  ("s" (info (al/src-dir-file "stumpwm/stumpwm.info")))
  ("o" (info "org"))
  ("g" (info "guile"))
  ("x" (info "guix"))
  ("M" (info "magit"))
  ("m" (info "make"))
  ("am" (info "automake"))
  ("ac" (info "autoconf"))
  ("t" (info "texinfo")))

;; Rebinding keys in `help-map' does not simply work: after evaluating
;; the code above, "C-h i" is still bound to `info'; resetting
;; `help-command' helps.
(fset 'help-command help-map)

(al/eval-after-load help-mode
  (al/bind-keys
    :map help-mode-map
    ("↷" help-go-back)
    ("↶" help-go-forward))

  (al/call-at-hook help-mode-hook al/no-truncate-lines))

(al/eval-after-load which-key
  (setq
   which-key-use-C-h-commands nil
   which-key-separator " "
   which-key-prefix-prefix ""
   which-key-idle-delay 0.8
   which-key-idle-secondary-delay 0.1
   which-key-add-column-padding 2
   which-key-max-display-columns 5))
(al/call-after-init which-key-mode)

(al/eval-settings-after-load
  (apropos (setq apropos-do-all t))
  (man "man")
  (woman "woman")
  (info "info")
  (texinfo "texinfo"))


;;; Spelling, translating

(al/bind-key "<XF86Spell>" tui/translate)

(al/bind-keys
 :prefix-map al/spell-map
 :prefix-doc "Map for flyspell and friends."
 :prefix-key "H-s"
 ("r" flyspell-region)
 ("b" flyspell-buffer)
 ("n" flyspell-goto-next-error)
 ("H-n" flyspell-goto-next-error))

(al/setq-no-warnings flyspell-use-meta-tab nil)

(al/eval-settings-after-load
  (ispell (ispell-change-dictionary "en" 'global))
  (flyspell "flyspell")
  (google-translate-core-ui "google-translate"))


;;; Time, calendar, diary, appointments, notifications, etc.

(al/bind-key* "M-T" tui/notification)

(al/bind-keys
 :prefix-map al/calendar-map
 :prefix-doc "Map for calendar, diary, notifications, etc."
 :prefix-key "M-C"
 ("M-C" calendar)
 ("c"   calendar)
 ("d"   diary)
 ("D"   al/diary-file)
 ("A"   appt-activate)
 ("a n" appt-add)
 ("a k" appt-delete))

;; `calendar-date-style' is used for other variables.
(al/setq-no-warnings calendar-date-style 'iso)

(al/eval-settings-after-load
  (time (setq
         display-time-interval 5
         display-time-format " %H:%M:%S"))
  (timer-list "timer-list")
  (calendar "calendar")
  (appt "appt")
  (al-notification
   (al/setq-file
    al/notification-sound (al/sound-dir-file "alarm.wav"))))


;;; Darts, journal

(al/bind-keys
 :prefix-map al/darts-map
 :prefix-doc "Map for darts and journal."
 :prefix-key "M-D"
 ("d" darts-day-template)
 ("s" darts-day-select)
 ("e" darts-day-export)
 ("M-S M-D" journal-search-by-date)
 ("M-S M-S" journal-grep)
 ("j" journal-create-entry)
 ("w" journal-position-windows)
 ("c" journal-change-created-property)
 ("v" journal-change-converted-property)
 ("b" journal-change-described-property)
 ("h" journal-insert-subheading)
 ("H" journal-back-to-entry-heading)
 ("i" journal-insert-block)
 ("t" (al/find-file (al/journal-dir-file "tags"))))

(al/eval-after-load journal
  (al/load-settings "journal"))

(al/autoload "darts-value"
  darts-throw-string-to-points
  darts-throw-string-to-code)

(al/autoload "darts-daydata"
  darts-day-template
  darts-day-select)

(al/eval-after-load darts-daydata
  (setq
   darts-database "darts"
   darts-data-dir "~/darts/daytables"
   darts-exported-dir (expand-file-name "exported" darts-data-dir)
   darts-template-file (expand-file-name "template" darts-data-dir)))


;;; Initial scratch and message buffers

(setq
 initial-major-mode #'emacs-lisp-mode
 initial-buffer-choice #'messages-buffer
 message-log-max 5000)

;; *scratch* buffer exists before `early-init-file' is loaded but it is
;; set up after `after-init-hook' is run if it still exists (see
;; `command-line').  Kill it, and set it up only when needed (on
;; `al/switch-to-scratch' call).
(and=> (get-buffer "*scratch*")
       #'kill-buffer)

(defun al/finalize-messages-buffer ()
  (with-current-buffer (messages-buffer)
    (messages-buffer-mode)
    (al/setq-no-warnings al/after-init-time (current-time))
    (insert "Config init time: " (al/init-time) "\n\f\n")))

(al/eval-at-hook messages-buffer-mode-hook
  (al/funcall 'hl-todo-mode)
  (setq buffer-read-only nil))

(al/call-after-init
  ;; This init hook should be the last one to set `al/after-init-time'.
  :depth 100
  al/finalize-messages-buffer)


;;; EMMS

(al/bind-keys
  :prefix-map al/emms-map
  :prefix-doc "Map for EMMS."
  :prefix-key [ctrl-m]
  ([ctrl-m] al/emms-switch-to-playlist-buffer)
  ("SPC" emms-pause)
  ("M-SPC" emms-stop)
  ("s"   al/emms-show)
  ("m"   emms-state-toggle-mode-line)
  ("n"   al/emms-notification-mode)
  ("B"   emms-browser)
  ("l"   (al/emms-playlist-select t))
  ("b"   al/emms-playlist-select)
  ("C-b" al/emms-playlist-select)
  ("r"   emms-streams)
  ("g"   al/emms-seek-to)
  ("y"   al/emms-mpv-sync-playing-time)
  ("S"   al/emms-save-playlists)
  ("u"   (emms-playlist-simple-uniq)))

(al/bind-keys
  :map al/emms-map
  :prefix-map al/emms-play-map
  :prefix-doc "Map for playing EMMS entries."
  :prefix-key "p"
  ("t" emms-play-directory-tree)
  ("d" emms-play-directory)
  ("f" emms-play-file)
  ("l" emms-play-playlist)
  ("u" emms-play-url))

(al/bind-keys
  :map al/emms-map
  :prefix-map al/emms-add-map
  :prefix-doc "Map for adding EMMS entries."
  :prefix-key "a"
  ("t" emms-add-directory-tree)
  ("d" emms-add-directory)
  ("f" emms-add-file)
  ("l" emms-add-playlist)
  ("u" emms-add-url))

(al/setq-no-warnings
 emms-directory (al/emacs-data-dir-file "emms")
 emms-playlist-sort-prefix "s")

(al/eval-after-load emms
  (al/load-settings "emms"))


;;; Internal (Emacs) and external (user or system) package managers

(al/bind-keys
  ("H-q" tui/package)
  ("H-x" guix))

(al/bind-keys
  :prefix-map al/guix-map
  :prefix-doc "Map for guix."
  :prefix-key "H-M-x"
  ("H-x" guix)
  ("f"   build-farm)
  ("e"   guix-edit)
  ("b"   guix-switch-to-buffer)
  ("P"   guix-prettify-mode)
  ("z"   guix-switch-to-repl)
  ("C-n" guix-packages-by-name)
  ("n"   guix-search-by-name)
  ("r"   guix-search-by-regexp)
  ("A"   guix-all-packages)
  ("N"   guix-newest-packages)
  ("I"   guix-installed-packages)
  ("O"   guix-obsolete-packages)
  ("G"   guix-generations)
  ("a"   guix-about)
  ("h"   guix-help)
  ("H"   guix-hash)
  ("p"   guix-profiles)
  ("H-p" guix-set-current-profile)
  ("i"   al/guix-switch-to-package-info-buffer)
  ("C-i" al/guix-switch-to-generation-info-buffer)
  ("l"   al/guix-switch-to-package-list-buffer)
  ("C-l" al/guix-switch-to-generation-list-buffer)
  ("u"   al/guix-commit-url))

(al/bind-keys
  :prefix-map al/aurel-map
  :prefix-doc "Map for aurel."
  :prefix-key "C-H-a"
  ("i"   al/switch-to-aurel-info)
  ("l"   al/switch-to-aurel-list)
  ("C-n" aurel-package-info)
  ("p"   aurel-package-search)
  ("n"   aurel-package-search)
  ("m"   aurel-maintainer-search)
  ("I"   aurel-installed-packages))

(al/setq-no-warnings
 quelpa-upgrade-p t
 quelpa-dir (al/emacs-data-dir-file "quelpa"))

(al/eval-settings-after-load
  (package "package")
  (guix "guix")
  (aurel "aurel"))


;;; Net settings: browsing, mail, chat, etc.

(al/bind-key* "M-S" tui/web-search)

(al/bind-keys
  :prefix-map al/net-map
  :prefix-doc "Map for net utils."
  :prefix-key "C-w"
  ("p" al/ping)
  ("t" al/traceroute)
  ("w" wget)
  ("m" al/url-wget-mp3))

(al/bind-keys
  :prefix-map al/gnus-map
  :prefix-doc "Map for Gnus."
  :prefix-key "M-g"
  ("M-g" al/gnus-switch-win-config)
  ("g"   al/gnus-switch-to-group-buffer)
  ("b"   al/gnus-switch-buffer)
  ("m"   gnus-msg-mail)
  ("n"   gnus-msg-mail))

(al/bind-keys*
  :prefix-map al/erc-map
  :prefix-doc "Map for ERC."
  :prefix-key "M-c"
  ("M-c" al/erc-track-switch-buffer)
  ("M-n" al/erc-cycle)
  ("l"   al/erc-channel-list)
  ("b"   al/erc-switch-buffer)
  ("M-s" al/erc-switch-to-server-buffer)
  ;; Non-interactive `erc' - compute everything without prompting:
  ("c"   (erc))
  ("R"   al/erc-server-buffer-rename)
  ("d"   al/erc-quit-server)
  ("j"   al/erc-join-channel)
  ("a"   al/erc-away)
  ("u"   al/erc-number-of-users)
  ("m"   erc-track-mode)
  ("n"   erc-notifications-mode)
  ("e"   (al/display-buffer "#emacs"))
  ("x"   (al/display-buffer "#guix"))
  ("s"   (al/display-buffer "#stumpwm"))
  ("M-z" (al/display-buffer "*status")))

(al/bind-keys
  :prefix-map al/debbugs-map
  :prefix-doc "Map for debbugs."
  :prefix-key "M-B"
  ("M-B" debbugs-gnu)
  ("n"   debbugs-gnu-bugs)
  ("b"   (al/display-buffer "*Guix-Patches Bugs*"))
  ("s"   debbugs-gnu-search))

(al/bind-keys
  :prefix-map al/debpaste-map
  :prefix-doc "Map for debpaste."
  :prefix-key "C-H-p"
  ("s" debpaste-paste-region)
  ("r" debpaste-display-paste)
  ("S" debpaste-display-posted-info-in-buffer)
  ("R" debpaste-display-received-info-in-buffer)
  ("d" debpaste-delete-paste)
  ("q" debpaste-quit-buffers)
  ("K" debpaste-kill-all-buffers))

(setq
 mail-user-agent 'gnus-user-agent
 user-full-name "Alex Kost")

(defvar al/mail-user-name "alezost")

(al/eval-after-init
  ;; Append because `al/mail-user-name' can be changed later.
  :depth 90
  (al/file-accessors "gnus"
    (al/emacs-data-dir-file (concat "gnus-" al/mail-user-name)))
  (al/setq-no-warnings
   ;; Set `gnus-home-directory' before loading Gnus.  Otherwise,
   ;; `gnus-startup-file' will be set to "~/.newsrc" for some reason.
   gnus-home-directory al/gnus-dir
   user-mail-address (concat al/mail-user-name "@gmail.com")))

(al/setq-no-warnings
 erc-modules
 '(autojoin
   button
   completion
   irccontrols
   keep-place
   list
   log
   match
   menu
   move-to-prompt
   netsplit
   networks
   nicks
   command-indicator
   notifications
   pcomplete
   readonly
   ring
   stamp
   track
   truncate))

;; Set it here (originally defined at `erc-log') to use below.
(defvar erc-log-channels-directory
  (al/emacs-data-dir-file "erc-log"))

;; Set `web-search-user-engines' here because the engines will be
;; generated during `web-search' package autoloading.
(al/setq-no-warnings
 web-search-user-engines
 '((ipduh "IPduh"
          "https://ipduh.com/apropos/?%s"
          web-search-clean-ip)
   (ip-address "IP address"
               "https://www.ip-address.org/lookup/ip-locator.php?track=%s"
               web-search-clean-ip)
   (yandex "Yandex"
           "https://yandex.ru/yandsearch?text=%s")
   (wikipedia-en "Wikipedia (english)"
                 "https://en.wikipedia.org/w/index.php?search=%s")
   (wikipedia-ru "Wikipedia (russian)"
                 "https://ru.wikipedia.org/w/index.php?search=%s")
   (youtube "Youtube"
            "https://www.youtube.com/results?search_query=%s&search=Search")
   (arch-package "Arch Packages"
                 "https://www.archlinux.org/packages/?sort=&q=%s&maintainer=&flagged=")
   (multitran-en/ru "Multitran en/ru"
                    "https://www.multitran.com/m.exe?l1=1&l2=2&s=%s")
   (multitran-ru/en "Multitran ru/en"
                    "https://www.multitran.com/m.exe?l1=2&l2=1&s=%s")
   (multitran-de/ru "Multitran de/ru"
                    "https://www.multitran.com/m.exe?l1=3&l2=2&s=%s")
   (multitran-ru/de "Multitran ru/de"
                    "https://www.multitran.com/m.exe?l1=2&l2=3&s=%s")
   (verbix-en "Verbix (en)"
              "https://verbix.com/webverbix/english/%s")
   (verbix-fr "Verbix (fr)"
              "https://verbix.com/webverbix/french/%s")
   (verbix-de "Verbix (de)"
              "https://verbix.com/webverbix/german/%s")
   (verbix-ko "Verbix (ko)"
              "https://verbix.com/webverbix/korean/%s")
   (verbix-ja "Verbix (ja)"
              "https://verbix.com/webverbix/japanese/%s")
   (naver-ru "Naver Dictionary (ko/ru)"
             "https://dict.naver.com/rukodict/#/search?query=%s")
   (naver-en "Naver Dictionary (ko/en)"
             "https://en.dict.naver.com/#/search?query=%s")))

(al/eval-settings-after-load
  (url (setq
        url-debug t
        url-configuration-directory (al/emacs-data-dir-file "url")))
  (browse-url "browse-url")
  (w3m "w3m")
  (wget (setq
         wget-debug-buffer "*wget-log*"
         wget-download-directory-filter 'wget-download-dir-filter-regexp
         wget-download-log-file (al/emacs-data-dir-file "emacs-wget.log")))
  (gnus "gnus")
  (sendmail (setq send-mail-function 'smtpmail-send-it))
  (smtpmail (setq smtpmail-smtp-server "smtp.gmail.com"
                  smtpmail-smtp-service 587))
  (shr "shr")
  (erc "erc")
  (erc-view-log
   (setq erc-view-log-timestamp-regexp
         (rx "[" (one-or-more (or digit ":")) "]")
         erc-view-log-timestamp-position 'left))
  (debpaste (setq debpaste-user-name "alezost"
                  debpaste-expire-time (* 3 24 60 60))
            (add-to-list 'debpaste-domains "debpaste" t))
  (debbugs-gnu "debbugs")
  (net-utils (setq ping-program-options '("-c" "3")))
  (al-net (setq
           al/net-hosts '("zeus" "leviafan" "hyperion" "192.168.1.1"
                          "google.com" "ya.ru")
           al/router-log-directory "~/docs/net/router-log/")))


;;; Visual settings: fonts, themes, mode-line, etc.

(al/bind-keys
  :prefix-map al/visual-map
  :prefix-doc "Map for visual stuff."
  :prefix-key "M-V"
  ("T"   tool-bar-mode)
  ("M"   menu-bar-mode)
  ("S"   scroll-bar-mode)
  ("I"   tooltip-mode)
  ("r"   rainbow-mode)
  ("t"   al/load-theme)
  ("C"   make-color)
  ("c"   make-color-switch-to-buffer)
  ("l"   (al/load-theme 'alect-light))
  ("M-l" (al/load-theme 'alect-light-alt))
  ("d"   (al/load-theme 'alect-dark))
  ("M-d" (al/load-theme 'alect-dark-alt))
  ("b"   (al/load-theme 'alect-black))
  ("M-b" (al/load-theme 'alect-black-alt))
  ("h"   hl-line-mode)
  ("w"   whitespace-mode)
  ("W"   global-whitespace-mode)
  ("M-W" (setq show-trailing-whitespace
               (not show-trailing-whitespace)))
  ("f"   al/face-to-kill-ring)
  ("F"   facemenu-set-foreground)
  ("B"   facemenu-set-background)
  ("M-F" make-color-foreground-color-to-kill-ring)
  ("M-B" make-color-background-color-to-kill-ring))

(setq
 frame-title-format '(al/server-running? server-name invocation-name)
 jit-lock-defer-time 0.1
 use-system-tooltips nil
 tooltip-delay 0.2)

(defface al/mode-name
  '((((background light)) :foreground "#028902")
    (((background dark))  :foreground "yellow"))
  "Face for `mode-name' displayed in the mode line.")

;; To have a server name of the running server in the mode-line, I use
;; an auxiliary variable `al/server-running?', because calling of
;; `server-running-p' in the mode-line construct eats CPU.  Idea of
;; right-aligning from
;; <http://lists.gnu.org/archive/html/help-gnu-emacs/2013-12/msg00191.html>
(defvar al/mode-server
  '(al/server-running?
    (:eval (list (propertize " "
                   'display `(space :align-to
                                    (- right ,(length server-name))))
                 server-name)))
  "Mode line construct for displaying `server-name' if server is running.")
(put 'al/mode-server 'risky-local-variable t)

(setq-default
 indicate-buffer-boundaries 'left
 visual-line-fringe-indicators '(nil vertical-bar)
 indicate-empty-lines t
 font-lock-extra-managed-props '(composition)

 mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   " " mode-line-buffer-identification
   " " mode-line-position
   " %l,%c"
   (vc-mode vc-mode)
   " " mode-line-modes
   mode-line-misc-info
   al/mode-server
   mode-line-end-spaces)

 mode-line-buffer-identification
 (propertized-buffer-identification "%b")

 mode-line-mule-info
 `(""
   (current-input-method
    (:propertize current-input-method-title
      help-echo (concat "Input method: " current-input-method "\n"
                        "mouse-2: Disable input method\n"
                        "mouse-3: Describe input method")
      local-map ,mode-line-input-method-map
      face font-lock-warning-face
      mouse-face mode-line-highlight))
   ,(propertize "%z"
      'help-echo 'mode-line-mule-info-help-echo
      'mouse-face 'mode-line-highlight
      'local-map mode-line-coding-system-map)
   (:eval (mode-line-eol-desc))))

(setq
 mode-line-position
 `((-3 ,(propertize "%P" 'face 'font-lock-builtin-face)))

 mode-line-modes
 (let ((recursive-edit-help-echo "Recursive edit")
       (mode-help-echo (concat "Mode actions:\n"
                               "mouse-1: Show menu\n"
                               "mouse-2: Show help\n"
                               "mouse-3: Minor modes")))
   (list '(:eval (al/mode-line-process-info))
         " "
         (propertize "%["
           'help-echo recursive-edit-help-echo
           'face 'font-lock-warning-face)
         "│"
         `(:propertize mode-name
            help-echo ,mode-help-echo
            face al/mode-name
            mouse-face mode-line-highlight
            local-map ,mode-line-major-mode-keymap)
         '(al/mode-info
           ("("
            (:propertize al/mode-info
              face font-lock-comment-face)
            ")"))
         `(:propertize minor-mode-alist
            mouse-face mode-line-highlight
            help-echo ,mode-help-echo
            local-map ,mode-line-minor-mode-keymap)
         `(:eval
           (if (buffer-narrowed-p)
               ,(propertize " ↕"
                  'help-echo "mouse-1: Remove narrowing"
                  'mouse-face 'mode-line-highlight
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 #'mode-line-widen))
             ""))
         "│"
         (propertize "%]"
           'help-echo recursive-edit-help-echo
           'face 'font-lock-warning-face))))

;; Make page breaks look fancier than the default "^L".
;; Idea from <http://www.jurta.org/en/emacs/dotemacs>.
(or standard-display-table
    (setq standard-display-table (make-display-table)))
(aset standard-display-table ?\^L
      (let ((line (make-vector 24 ?—)))
        (vconcat line " page break " line)))

(column-number-mode)
(blink-cursor-mode 0)
;; (mouse-avoidance-mode 'banish)

;; MenuBar, ToolBar, and ScrollBar are already disabled at
;; "~/.Xresources" file.  Disabling them here is already too late for
;; Emacs startup time (the bars appear for a moment on Emacs start and
;; disappear if they are disabled in Emacs config).  So I want to see
;; these bars if something went wrong with Xresources.
;;
;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)

(al/call-after-init show-paren-mode)

(al/eval-after-load custom
  (setq custom-safe-themes t)

  ;; Fix bug <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16266>.
  (defun al/fix-custom-variables-bug (fun &rest args)
    "Allow setting undefined variables in themes."
    (let (custom--inhibit-theme-enable)
      (apply fun args)))
  (advice-add 'custom-theme-set-variables
    :around 'al/fix-custom-variables-bug))

(al/eval-after-load alect-themes
  (setq
   alect-display-class '((class color) (min-colors 256))
   alect-overriding-faces
   '((hl-line ((((type graphic)) :background bg)
               (t :background unspecified))))))

(al/eval-after-load dim
  :load 'after-init
  (dim-major-names
   '((emacs-lisp-mode            "EL")
     (elisp-byte-code-mode       "EL-byte")
     (lisp-interaction-mode      "ELi")
     (inferior-emacs-lisp-mode   "EL>")
     (lisp-mode                  "CL")
     (slime-repl-mode            "CL>")
     (scheme-mode                "λ")
     (geiser-repl-mode           "λ>")
     (geiser-doc-mode            "λ🄷")
     (python-mode                "Py")
     (inferior-python-mode       "Py>")
     (js-mode                    "JS")
     (sh-mode                    "Sh")
     (shell-mode                 "Sh>")
     (eshell-mode                "ESh>")
     (dired-mode                 "🗀")
     (wdired-mode                "🗁")
     (Info-mode                  "🄸")
     (help-mode                  "🄷")
     (doc-view-mode              "Doc")
     (pdf-view-mode              "pdf-View")
     (pdf-outline-buffer-mode    "pdf🖹")
     (sql-interactive-mode       "SQL>")
     (ibuffer-mode               "IB")
     (message-mode               "🖂")
     (erc-view-log-mode          "ERC🄻")
     (erc-list-menu-mode         "ERC🗋")
     (calc-mode                  "=")
     (debugger-mode              "🔨")
     (snippet-mode               "🗍")
     (diary-mode                 "🕮")
     (ediff-mode                 "ε")
     (xref--xref-buffer-mode     "xref")

     (gnus-server-mode           "𝗚Srv")
     (gnus-browse-mode           "𝗚Srv🗋")
     (gnus-group-mode            "𝗚Gr")
     (gnus-summary-mode          "𝗚Sum")
     (gnus-article-mode          "𝗚Art")

     (guix-package-info-mode     "γ🄷pkg")
     (guix-generation-info-mode  "γ🄷gen")
     (guix-package-list-mode     "γ🗋pkg")
     (guix-output-list-mode      "γ🗋out")
     (guix-generation-list-mode  "γ🗋gen")
     (guix-profile-list-mode     "γ🗋prof")
     (guix-build-log-mode        "γ🄻")

     (magit-status-mode          "µStatus")
     (magit-process-mode         "µProc")
     (magit-log-mode             "µ🄻")
     (magit-log-select-mode      "µ🄻Select")
     (magit-reflog-mode          "µReflog")
     (magit-refs-mode            "µRefs")
     (magit-diff-mode            "µDiff")
     (magit-revision-mode        "µRevision")
     (magit-cherry-mode          "µCherry")
     (magit-stash-mode           "µStash")
     (magit-stashes-mode         "µStashes")
     (magit-popup-mode           "µPopup")
     (magit-popup-sequence-mode  "µPopupSeq")
     (git-rebase-mode            "git-Rebase")
     (gitconfig-mode             "git-Config")
     (gitignore-mode             "git-Ignore")
     (gitattributes-mode         "git-Attributes")

     (calendar-mode              "📆")
     (w3m-form-input-select-mode "w3m🗹")
     (package-menu-mode          "Pkg🗋")
     (emms-playlist-mode         "🎝")
     (emms-stream-mode           "🎝 Streams")
     (sauron-mode                "👁")))

  (dim-minor-names
   '((visual-line-mode           " ↩")
     (auto-fill-function         " ↵")
     (isearch-mode               " 🔎")
     (whitespace-mode            " _"           whitespace)
     (rainbow-mode               " 🖌"           rainbow-mode)
     (abbrev-mode                " Ab"          abbrev)
     (company-mode               " ⍈"           company)
     (yas-minor-mode             " ⮞"           yasnippet)
     (paredit-mode               " PE"          paredit)
     (view-mode                  " 👀"           view)
     (eldoc-mode                 ""             eldoc)
     (edebug-mode                " 🔧"           edebug)
     (counsel-mode               ""             counsel)
     (pdf-view-themed-minor-mode ""             pdf-view)

     (gnus-topic-mode            " T"           gnus-topic)
     (gnus-dired-mode            " 𝗚"           gnus-dired)

     (guix-build-log-minor-mode  " γ🄻"          guix-build-log)
     (guix-devel-mode            " γ"           guix-devel)

     (magit-blame-mode           " µBlame"      magit-blame)
     (erc-notifications-mode     " 🗩"           erc-desktop-notifications)
     (al/emms-notification-mode  " 🎧"           al/emms)
     (flyspell-mode              " fly"         flyspell))))

(al/eval-settings-after-load
  (scroll-bar (setq previous-scroll-bar-mode 'right))
  (whitespace "whitespace")
  (paren (setq show-paren-delay 0.1
               show-paren-when-point-inside-paren t
               show-paren-when-point-in-periphery t))
  (ruler-mode (setq ruler-mode-show-tab-stops t))
  (rainbow-mode (setq rainbow-x-colors t))
  (make-color (al/call-at-hook make-color-mode-hook
                al/bar-cursor-type)))

(al/eval-after-frame-init
  :name al/graphical-frame-visual-settings
  :terminal graphical
  :once t
  (al/eval-after-load al-visual
    :load t
    (when (al/require alect-themes)
      (al/load-theme 'alect-light))
    ;; Should be "solved":
    ;; 한글, ひらがな, 漢字, ＃＊ (droid);
    ;; 🐼, 😻, ⚽, 💩, ∵, ⸪, 🃜, 🜒, 🝖, ←↑→↓ (symbola);
    ;; ࿌ (unifont).
    (setq use-default-font-for-symbols nil)
    (let ((font (al/first-existing-font
                 "Liberation Mono-12"
                 "DejaVu Sans Mono-11"
                 "Terminus-12")))
      (set-frame-font font nil t)
      (al/set-fontset
        (font 'greek)
        ("Droid Sans Mono" 'han 'hangul 'kana 'cjk-misc)
        ;; Setting nil is needed to display unknown symbols (like ￰)
        ;; properly i.e., without using Droid fallback.
        ("Symbola" 'mathematical 'symbol nil)))))


;;; Misc settings and packages

(al/bind-keys
  :map mule-keymap
  ("d" (revert-buffer-with-coding-system 'cp855))
  ("w" (revert-buffer-with-coding-system 'cp1251)))

(al/bind-keys-from-vars 'special-mode-map 'al/lazy-moving-keys t)

(al/bind-keys
  :prefix-map al/org-map
  :prefix-doc "Map for org mode."
  :prefix-key "M-r"
  ("M-r" org-insert-link)
  ("l"   org-store-link)
  ("M-l" org-store-link)
  ("M-b" org-mark-ring-goto)
  ("c"   org-capture)
  ("a"   org-agenda)
  ("b"   org-switchb)
  ("i"   org-toggle-inline-images)
  ("e"   org-export)
  ([tab] org-indent-mode))

(al/autoload "pdf-view" pdf-view-mode)

(setq-default
 major-mode 'text-mode
 truncate-lines t
 indent-tabs-mode nil
 tab-always-indent t
 fill-column 72)

(setq
 ;; Smooth scrolling.
 mouse-wheel-scroll-amount '(3 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 scroll-conservatively 111
 auto-window-vscroll nil
 next-screen-context-lines 3
 scroll-preserve-screen-position t

 use-short-answers t
 save-abbrevs nil
 password-cache-expiry (* 24 60 60)
 line-number-display-limit-width 9999
 echo-keystrokes 0.2
 disabled-command-function nil
 inhibit-startup-screen t

 ;; Set `find-function-C-source-directory' instead of
 ;; `source-directory'.  Otherwise, "trampver.el" will run "git" (twice)
 ;; on load to set `tramp-repository-branch' and
 ;; `tramp-repository-version' v̶a̶r̶i̶a̶b̶l̶e̶s̶ constants (so
 ;; setting them here won't help since `defconst' always reevaluates).
 source-directory nil
 find-function-C-source-directory (al/src-dir-file "emacs/src")

 enable-local-variables :safe
 enable-dir-local-variables nil
 ;; safe-local-variable-values '((lexical-binding . t))
 ;; enable-local-eval nil

 warning-minimum-level :warning
 warning-suppress-types      ; do not pop up the *Warnings* buffer when:
 '(;; something long is executed in *shell*.
   (undo discard-info)))

(prefer-coding-system 'utf-8)
(al/call-at-hook after-save-hook al/check-parens)

(al/call-at-hook (delete-frame-functions
                  kill-emacs-hook)
  al/save-everything)

(electric-indent-mode 0)

;; `normal-mode' should always be called with t argument, otherwise
;; it simply ignores the value of `enable-local-variables' and sets
;; it to t.
(defun al/fix-normal-mode (&rest _)
  (list t))
(advice-add 'normal-mode :filter-args #'al/fix-normal-mode)

(al/bind-keys
  :map process-menu-mode-map
  :check t
  ("C-k" process-menu-delete-process))

(al/eval-after-load al-process
  :load after-init
  (advice-add 'insert-directory :around #'al/call-with-locale)
  (al/process-hook-mode))

(al/eval-after-load server
  (setq
   server-kill-new-buffers nil
   server-temp-file-regexp
   (concat server-temp-file-regexp
           "\\|COMMIT_EDITMSG\\|git-rebase-todo")))

(al/eval-after-load al-server
  :load after-init
  (advice-add 'server-visit-files :around #'al/autoload-org-protocol)
  (when-let* ((name (al/server-name)))
    (setq al/server-running? t)
    (when (equal name "emms")
      (setq initial-major-mode
            (lambda (&rest _) (text-mode) (al/text-scale+1)))
      (al/with-check
        :var '(al/mail-user-name   ; defined in "net.el"
               al/mail-user-name2) ; defined in "custom.el"
        (with-no-warnings
          (setq al/mail-user-name
                al/mail-user-name2)))
      (al/funcall 'al/save-place-mode)
      (al/funcall 'al/recentf-mode)
      (al/funcall 'appt-activate))))

(al/eval-after-load text-mode
  (al/modify-syntax text-mode-syntax-table (?\" "\"   "))

  (al/call-at-hook text-mode-hook
    visual-line-mode
    hl-line-mode
    abbrev-mode
    al/no-syntactic-font-lock
    al/show-trailing-whitespace))

(al/eval-after-load tabulated-list
  ;; Not putting this into separate file because
  ;; (featurep 'tabulated-list) evaluates to t
  ;; even in "early-init.el"

  (defconst al/tabulated-list-keys
    '(("s" tabulated-list-sort))
    "Alist of auxiliary keys for `tabulated-list-mode-map'.")
  (al/bind-keys-from-vars 'tabulated-list-mode-map
    '(al/lazy-moving-keys al/tabulated-list-keys)
    t)

  (add-hook 'tabulated-list-mode-hook #'hl-line-mode))

;; Default value of `tramp-ssh-controlmaster-options' variable slows
;; down loading tramp significantly.  This should be set before tramp
;; is loaded.
(al/setq-no-warnings tramp-ssh-controlmaster-options "")

(al/add-to-auto-mode-alist
  (sh-mode "/etc/profile\\'"
           "bashrc\\'")
  (conf-xdefaults-mode (al/file-regexp "Xmodmap"))
  (conf-space-mode (al/file-regexp "mailmap" "gitignore"))
  (conf-unix-mode (al/file-regexp
                    "rules" "hwdb" "cnf" "map" "inc" "service"
                    "target" "socket" "timer" "mount"))
  (conf-unix-mode append
                  ".*rc\\'"
                  "/etc/.*\\'")
  (js-mode "/etc/polkit-1/rules\\.d/.+\\.rules")
  (syslog-mode append "/var/log.*\\'"
               ;;"\\.log\\'" not this because of ~/config/emacs/data/emacs-wget.log
               )
  (erc-view-log-mode (concat "\\`"
                             (regexp-quote (expand-file-name
                                            erc-log-channels-directory))))
  (zapret-nfqws-mode "zapret.*\\.conf\\'")
  (emacs-lisp-mode "/emms/.+\\.pl\\'") ; my playlists in `emms-directory'
  (pdf-view-mode "\\.[pP][dD][fF]\\'")
  (markdown-mode (al/file-regexp "mdown"))
  (pkgbuild-mode "PKGBUILD\\'")
  (java-mode ".*tmwa-server-data/world/map/npc/.*txt\\'")
  (gtypist-mode (al/file-regexp "typ"))
  (gnuplot-mode (al/file-regexp "plot"))
  (maxima-mode (al/file-regexp "max")))

(al/eval-settings-after-load
  (mwim "mwim")
  (imenu "imenu")
  (imenus "imenus")
  (comp-run
   (setq native-comp-async-warnings-errors-kind 'all
         ;; Native compilation is useless for files with variable
         ;; settings, key definitions, etc.  Moreover, almost all my
         ;; settings files will not be compiled by JIT compiler anyway
         ;; because they contain no functions or macros.
         native-comp-jit-compilation-deny-list
         (list (regexp-opt (list al/emacs-settings-dir)))))
  (tramp-sh
   (push 'tramp-own-remote-path tramp-remote-path)
   (push "LC_ALL=en_US.UTF-8" tramp-remote-process-environment)
   (push "DISPLAY=:0" tramp-remote-process-environment))
  (gnutls
   ;; http://comments.gmane.org/gmane.emacs.gnus.general/83413
   (setq gnutls-min-prime-bits nil))
  (calc (setq calc-angle-mode 'rad))
  (picture "picture")
  (artist "artist")
  (hexl "hexl")
  (diff-mode "diff-mode")
  (ediff "ediff")
  (view "view")
  (conf-mode (add-hook 'conf-mode-hook #'hl-line-mode))
  (image-mode "image-mode")
  (doc-view
   (push "-r200" doc-view-ghostscript-options) ; picture resolution
   (setq doc-view-cache-directory "~/.cache/docview"))
  (markdown-mode "markdown-mode")
  (org "org")
  (tex-mode "tex-mode")
  (pdf-view "pdf-tools")
  (tar-mode "tar-mode")
  (nxml-mode "nxml-mode")
  (sql "sql")
  (bui "bui")
  (transient "transient")
  (epa "epa")
  (xref "xref")
  (gamegrid (setq gamegrid-user-score-file-directory
                  (al/emacs-data-dir-file "games")))
  (ducpel "ducpel")
  (tetris "tetris")
  (snake "snake"))

;;; settings.el ends here
