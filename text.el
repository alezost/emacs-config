;;; text.el --- Working with text: editing, searching, …

;; Copyright © 2014-2015 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Global keys for moving

(bind-keys
 ("C-o"   . backward-char)
 ("M-o"   . backward-word)
 ("C-M-o" . backward-sexp)
 ("M-O"   . backward-sentence)
 ("C-H-o" . (lambda () (interactive) (scroll-right 1)))

 ("C-u"   . forward-char)
 ("M-u"   . forward-word)
 ("C-M-u" . forward-sexp)
 ("M-U"   . forward-sentence)
 ("C-H-u" . (lambda () (interactive) (scroll-left 1)))

 ("C-."   . previous-line)
 ("M-."   . backward-paragraph)
 ("C-M-." . backward-up-list)
 ("M->"   . backward-page)
 ("C-H-." . (lambda () (interactive) (scroll-down 1)))
 ("H-."   . scroll-down-command)
 ("H-M-." . scroll-other-window-down)
 ("s-."   . utl-previous-link)

 ("C-e"   . next-line)
 ("M-e"   . forward-paragraph)
 ("C-M-e" . down-list)
 ("M-E"   . forward-page)
 ("C-H-e" . (lambda () (interactive) (scroll-up 1)))
 ("H-e"   . scroll-up-command)
 ("H-M-e" . scroll-other-window)
 ("s-e"   . utl-next-link)

 ("C-a"   . mwim-beginning-of-code-or-line)
 ("C-M-a" . beginning-of-defun)
 ("M-A"   . utl-beginning-of-line)
 ("H-a"   . beginning-of-buffer)

 ("C-п"   . mwim-end-of-code-or-line)
 ("C-M-i" . end-of-defun)
 ("M-I"   . utl-end-of-line)
 ("H-i"   . end-of-buffer)

 ("C-3"   . recenter-top-bottom)
 ("C-H-3" . utl-recenter-top)
 ("C-2"   . move-to-window-line-top-bottom))

(unless (display-graphic-p)
  (bind-keys
   ("M-."   . previous-line)
   ("M-e"   . next-line)
   ("M-a"   . utl-beginning-of-code-or-line)
   ("M-i"   . utl-end-of-code-or-line)
   ("M->"   . scroll-down-command)
   ("M-E"   . scroll-up-command)
   ("C-x a" . beginning-of-buffer)
   ("C-x i" . end-of-buffer)
   ("C-M-i" . complete-symbol)))


;;; Global keys for editing

(bind-keys
 ("C-,"   . delete-char)
 ("M-,"   . kill-word)
 ("C-M-," . kill-sexp)
 ("M-<"   . kill-line)
 ("H-M-," . utl-delete-blank-lines)

 ("C-p"   . delete-backward-char)
 ("M-p"   . backward-kill-word)
 ("C-M-p" . backward-kill-sexp)
 ("M-P"   . utl-backward-kill-line)
 ("H-M-p" . delete-trailing-whitespace)

 ("C-M-q" . utl-save-sexp)
 ("M-Q"   . utl-save-line)

 ("C-k"   . kill-whole-line)
 ("M-k"   . utl-save-whole-line)
 ("C-M-k" . utl-backward-save-sexp)
 ("M-K"   . utl-backward-save-line)
 ("H-k"   . utl-duplicate-dwim)

 ("C-'"   . transpose-chars)
 ("M-'"   . (lambda () (interactive) (transpose-words -1)))
 ("C-M-'" . transpose-sexps)
 ("M-\""  . transpose-lines)

 ("C-;"   . open-line)
 ("M-;"   . utl-comment-dwirm)
 ("C-M-;" . split-line)

 ("C-t"   . kill-region)
 ("M-x"   . kill-ring-save)
 ("C-M-x" . append-next-kill)

 ("S-SPC"     . just-one-space)
 ("M-S-SPC"   . utl-delete-horizontal-space)
 ("M-SPC"     . mark-word)
 ("M-s-SPC"   . mark-paragraph)
 ("C-M-s-SPC" . mark-defun)
 ("H-s-SPC"   . mark-whole-buffer)

 ("C-j" . newline-and-indent)
 ("C-M-y" . utl-insert-clipboard)
 ("<S-backspace>" . delete-region)
 ("H-M-a" . align-regexp)
 ("C-H-M-a" . (lambda () (interactive)
                (align-regexp (region-beginning) (region-end)
                              "\\(\\s-*\\)((")))
 ("M-%" . ispell-complete-word)
 ("M-_" . utl-number-down)
 ("M-+" . utl-number-up)

 ("M-/"   . dabbrev-expand)
 ("C-M-/" . hippie-expand)
 ("M-?"   . utl-dabbrev-expand-word)

 ("<C-kanji>"   . utl-downcase-word-backward)
 ("<S-kanji>"   . utl-capitalize-word-backward)
 ("<H-kanji>"   . utl-upcase-word-backward)
 ("<C-M-kanji>" . utl-downcase-dwim)
 ("<M-S-kanji>" . utl-capitalize-dwim)
 ("<H-M-kanji>" . utl-upcase-dwim))

(bind-keys
 ("<M-tab>" . complete-symbol) ; shadows "\M-\t" ("C-M-i") in any mode, so be careful
 ("<C-tab>" . indent-relative)
 ("<M-S-iso-lefttab>" . tab-to-tab-stop)
 ("<H-tab>" . indent-region)
 ("C-c u" . utl-decode-region))


;;; Global keys for inserting text

(define-key key-translation-map [?\C--] [?–])
(define-key key-translation-map [?\C-\M--] [?—])

(bind-keys
 ("C->"   . (lambda () (interactive) (insert "->")))
 ("H-4"   . insert-parentheses)
 ("H-5"   . insert-pair-square-brackets)
 ("H-6"   . insert-pair-curly-brackets)
 ("H-,"   . insert-pair-angle-brackets)
 ("H-;"   . insert-pair-single-quotations)
 ("H-'"   . insert-pair-double-quotations)
 ("C-H-," . insert-pair-angle-quotations)
 ("C-H-;" . insert-pair-left-right-single-quotations)
 ("C-H-'" . insert-pair-left-right-double-quotations)
 ("H-`"   . insert-pair-grave-accent-quotation)
 ("C-H-`" . insert-pair-grave-accents))

(bind-keys
 :prefix-map al/insert-map
 :prefix-docstring "Map for inserting symbols and text."
 :prefix "M-i"
 ("M-i" . utl-insert-delimiter)
 ("M-a" . (lambda () (interactive) (insert ";;;###autoload\n")))
 ("t"   . (lambda () (interactive) (insert "TODO ")))
 ("f"   . (lambda () (interactive) (insert "FIXME ")))
 ("d"   . utl-insert-date)
 ("M-'" . insert-pair-top-corners)
 ("M-;" . insert-pair-bottom-corners))


;;; Searching, finding and replacing

(bind-keys
 :map search-map
 ("s"   . query-replace)
 ("M-s" . query-replace)
 ("r"   . query-replace-regexp)
 ("R"   . replace-regexp))

(setq isearch-allow-scroll t)
(bind-keys
 :map isearch-mode-map
 ("M-s" . isearch-query-replace)
 ("M-d" . isearch-edit-string)
 ("M-o" . isearch-occur)
 ("s-7" . (lambda () (interactive)
            (utl-set-isearch-input-method nil)))
 ("s-8" . (lambda () (interactive)
            (utl-set-isearch-input-method "dvorak-russian-computer"))))

(defconst al/occur-keys
  '(("." . occur-prev)
    ("e" . occur-next)
    ("u" . occur-mode-goto-occurrence))
  "Alist of auxiliary keys for `occur-mode-map'.")
(al/bind-keys-from-vars 'occur-mode-map 'al/occur-keys)

(defun al/occur-set-paragraph ()
  "Set paragraph to be started from any non-space symbol."
  (setq-local paragraph-start "[^ ]"))
(al/add-hook-maybe 'occur-mode-hook 'al/occur-set-paragraph)

(use-package misearch
  :defer t
  :config
  (setq multi-isearch-pause nil))

(use-package point-pos
  :defer t
  :init
  (al/add-my-package-to-load-path-maybe "point-pos")
  (bind-keys
   :prefix-map al/point-pos-map
   :prefix-docstring "Map for point-pos."
   :prefix "M-Z"
   ("s" . point-pos-save)
   ("d" . point-pos-delete)
   ("g" . point-pos-goto)
   ("h" . point-pos-previous)
   ("n" . point-pos-next))
  (bind-keys
   ("C-M-S-g" . point-pos-goto)
   ("C-M-S-h" . point-pos-previous)
   ("C-M-S-n" . point-pos-next)))

(use-package imenu
  :defer t
  :init
  (bind-key* "C-M-s-m" 'imenu)
  :config
  (setq imenu-space-replacement nil))

(use-package imenus
  :defer t
  :init
  (al/add-my-package-to-load-path-maybe "imenus")
  (bind-key* "C-M-m" 'imenus)
  :config
  (setq imenus-delimiter " ⇨ ")
  (bind-keys
   :map imenus-minibuffer-map
   ("C-r" . imenus-rescan)
   ("C-s" . imenus-exit-to-isearch)
   ("M-s" . imenus-exit-to-occur)))

(use-package utl-imenus
  :defer t
  :init
  (bind-key "s-s" 'utl-imenus-search-elisp-dir)
  :config
  (setq utl-imenus-elisp-dir al/emacs-init-dir))


;;; Killing and yanking

(setq
 mark-ring-max 30
 set-mark-command-repeat-pop t
 mouse-yank-at-point t
 kill-do-not-save-duplicates t
 mouse-drag-copy-region t
 select-active-regions nil)

;; Use PRIMARY instead of CLIPBOARD.
(if (version< emacs-version "25")
    (setq x-select-enable-primary t
          x-select-enable-clipboard nil)
  (setq select-enable-primary t
        select-enable-clipboard nil))

(use-package browse-kill-ring
  :defer 5
  :config
  (setq
   browse-kill-ring-separator (make-string 64 ?—)
   browse-kill-ring-separator-face nil)
  (defconst al/browse-kill-ring-keys
    '(("."   . browse-kill-ring-previous)
      ("e"   . browse-kill-ring-forward)
      ("u"   . browse-kill-ring-insert-and-quit)
      ("M-d" . browse-kill-ring-edit)
      ("C-g" . browse-kill-ring-quit))
    "Alist of auxiliary keys for `browse-kill-ring-mode-map'.")
  ;; The keys are defined inside `browse-kill-ring-mode'.
  (defun al/browse-kill-ring-bind-keys ()
    (al/bind-keys-from-vars 'browse-kill-ring-mode-map
      'al/browse-kill-ring-keys t))
  (al/add-hook-maybe 'browse-kill-ring-mode-hook
    'al/browse-kill-ring-bind-keys)
  (browse-kill-ring-default-keybindings))

(use-package register
  :defer t
  :config
  (setq register-preview-delay 0.3)

  (defun al/insert-register-reverse-arg (fun register &optional arg)
    "Reverse the meaning of ARG for `insert-register'."
    (funcall fun register (not arg)))
  (advice-add 'insert-register
    :around #'al/insert-register-reverse-arg))


;;; Misc settings and packages

(setq-default
 truncate-lines t
 indent-tabs-mode nil
 fill-column 72)

(setq
 save-abbrevs nil
 dabbrev-abbrev-char-regexp "\\sw\\|[-_+*]")
(setq parens-require-spaces nil)

;; Smooth scrolling.
(setq
 mouse-wheel-scroll-amount '(3 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 scroll-conservatively 111
 auto-window-vscroll nil
 next-screen-context-lines 3
 scroll-preserve-screen-position t)

(prefer-coding-system 'utf-8)
(al/add-hook-maybe 'after-save-hook 'utl-check-parens)

(al/add-hook-maybe 'text-mode-hook
  '(visual-line-mode
    abbrev-mode
    al/show-trailing-whitespace))
(al/bind-keys-from-vars 'text-mode-map 'al/text-editing-keys)

(use-package abbrev
  :defer t
  :diminish " Ab"
  :config
  (define-abbrev-table 'global-abbrev-table
    '(("gos"  "GuixSD")
      ("hhg"  "GNU/Linux")
      ("hhpr" "programming")
      ("hhy"  "yesterday")
      ("hhY"  "Yesterday")
      ("hh1"  "Monday")
      ("hh2"  "Tuesday")
      ("hh3"  "Wednesday")
      ("hh4"  "Thursday")
      ("hh5"  "Friday")
      ("hh6"  "Saturday")
      ("hh7"  "Sunday"))))

(use-package iso-transl
  :defer t
  :init
  (define-key key-translation-map [?\M-i] 'iso-transl-ctl-x-8-map)
  :config
  ;; Expand "C-x 8" (now "M-i") map:
  (iso-transl-define-keys
   '(("a"        . [?α])
     ("b"        . [?β])
     ("g"        . [?γ])
     ("l"        . [?λ])
     ("p"        . [?π])
     ("i"        . [?∞])
     ("s"        . [?☺])
     ("S"        . [?☹])
     ("="        . [?≈])
     ("' "       . [?′])
     ("\" "      . [?″])
     ("/12"      . [?½])
     ("/13"      . [?⅓])
     ("/23"      . [?⅔])
     ("/14"      . [?¼])
     ("/34"      . [?¾])
     ([?\C-.]    . [?·])
     (">"        . [?•])
     ("."        . [?…])
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
    (cl-loop for i to 9
             for char across numbers
             collect (cons (kbd (concat modifier
                                        (number-to-string i)))
                           (vector char))))
  (iso-transl-define-keys
   (append
    (al/make-number-alist "⁰¹²³⁴⁵⁶⁷⁸⁹")
    (al/make-number-alist "₀₁₂₃₄₅₆₇₈₉" "M-"))))

(use-package pcomplete
  :defer t
  :config
  ;; Although `pcomplete-suffix-list' is marked as obsolete, it is used
  ;; by `pcomplete-insert-entry', and its default value prevents
  ;; inserting space after ":" (while completing ERC nicks).
  (setq pcomplete-suffix-list nil)

  (when (require 'utl-pcomplete nil t)
    (al/add-hook-maybe '(shell-mode-hook eshell-mode-hook)
      'utl-pcomplete-no-space)))

(use-package pcmpl-args
  :defer t
  :config
  (setq
   pcmpl-args-debug-parse-help t
   pcmpl-args-cache-default-duration 999999
   pcmpl-args-cache-max-duration pcmpl-args-cache-default-duration))

(use-package company
  :defer t
  :diminish " ⍈"
  :commands company-complete
  :init
  (bind-key "<C-H-tab>" 'company-complete)
  :config
  (setq
   company-idle-delay nil
   company-show-numbers t)
  (bind-keys
   :map company-active-map
   ("M-." . company-select-previous)
   ("M-e" . company-select-next))
  (global-company-mode))

(use-package yasnippet
  ;; I do not use `yas-minor-mode' (or `yas-global-mode') because I don't
  ;; want to see `yas--post-command-handler' in `post-command-hook'.  I
  ;; just use yas functionality when I need to expand something without
  ;; enabling the mode.
  :defer t
  :diminish (yas-minor-mode . " ⮞")
  :commands
  (yas-new-snippet yas-insert-snippet)
  :init
  (setq
   al/my-snippets-dir  (al/emacs-data-dir-file "snippets/my")
   al/yas-snippets-dir (al/emacs-data-dir-file "snippets/yas")
   ;; al/yas-snippets-dir (expand-file-name "yasnippet/snippets"
   ;;                                       quelpa-build-dir)
   yas-snippet-dirs (list al/my-snippets-dir al/yas-snippets-dir)
   yas-prompt-functions '(yas-ido-prompt))

  (bind-keys
   ("<kanji>"   . utl-yas-next-field-or-expand)
   ("<M-kanji>" . utl-yas-exit-and-expand))
  (bind-keys
   :prefix-map al/yas-map
   :prefix-docstring "Map for yasnippet commands."
   :prefix "M-Y"
   ("M-Y" . yas-insert-snippet)
   ("f"   . yas-visit-snippet-file)
   ("r"   . (lambda () (interactive) (yas--load-pending-jits)))
   ("R"   . yas-reload-all)
   ("n"   . yas-new-snippet)
   ("l"   . yas-load-directory)
   ("d"   . yas-describe-tables)
   ("g"   . yas-global-mode)
   ("s"   . (lambda () (interactive)
              (utl-ido-find-file
               (expand-file-name "yasnippet/snippets/emacs-lisp-mode"
                                 quelpa-build-dir)))))

  :config
  (setq yas-indent-line 'fixed)
  (setq yas-new-snippet-default "\
# -*- mode: snippet; require-final-newline: nil -*-
# contributor: Alex Kost
# name: $1
# key: $1
# --
$0")

  ;; Allow any editing during working with a snippet.
  (advice-add 'yas--on-protection-overlay-modification
    :override 'ignore)

  (bind-key "C-g" (lambda () (interactive)
                    (yas-exit-all-snippets) (keyboard-quit)))
  (bind-keys
   :map yas-minor-mode-map
   ("<tab>") ("TAB"))
  (bind-keys
   :map yas-keymap
   ("<kanji>"   . yas-next-field-or-maybe-expand)
   ("<S-kanji>" . yas-prev-field)
   ("M-<"       . yas-skip-and-clear-or-delete-char)
   ("C-d") ("C-g") ("<tab>") ("TAB"))
  (bind-keys
   :map snippet-mode-map
   ("C-c C-k" . (lambda () (interactive) (kill-buffer nil))))

  (yas--load-snippet-dirs)
  (yas--load-pending-jits))

(use-package paredit
  :defer t
  :diminish " ()"
  :config
  (al/clean-map 'paredit-mode-map)
  (bind-keys
   :map paredit-mode-map
   ("<H-M-tab>" . paredit-reindent-defun)
   ("C-j"       . paredit-newline)
   ("M-p"       . paredit-backward-kill-word)
   ("M-,"       . paredit-forward-kill-word)
   ("C-M-o"     . paredit-backward)
   ("C-M-u"     . paredit-forward)
   ("C-M-."     . paredit-backward-up)
   ("C-M-e"     . paredit-forward-down)
   ("M-<"       . paredit-kill)
   ("H->"       . paredit-splice-sexp)
   ("H-P"       . paredit-splice-sexp-killing-backward)
   ("H-<"       . paredit-splice-sexp-killing-forward)
   ("C-M-l"     . paredit-raise-sexp)
   ("C-)"       . paredit-forward-slurp-sexp)
   ("C-M-0"     . paredit-forward-barf-sexp)
   ("C-("       . paredit-backward-slurp-sexp)
   ("C-M-9"     . paredit-backward-barf-sexp)
   ("C-M-5"     . paredit-split-sexp)
   ("C-M-6"     . paredit-join-sexps))
  (add-to-list 'emulation-mode-map-alists
               `((paredit-mode . ,paredit-mode-map))))

;;; text.el ends here
