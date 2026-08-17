;;; magit.el --- Settings for `magit' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'magit)
(require 'al-key)
(require 'al-magit)

(defconst al/magit-common-keys
  '(("v"   . magit-git-command)
    "M-m")
  "Alist of auxiliary keys that should be bound in any magit mode.")

(defconst al/magit-history-keys
  '((","   . magit-go-backward)
    ("p"   . magit-go-forward))
  "Alist of auxiliary keys for moving by magit history.")

(defconst al/magit-scroll-diff-keys
  '(("SPC" . magit-diff-show-or-scroll-up)
    ("DEL" . magit-diff-show-or-scroll-down))
  "Alist of auxiliary keys for scrolling magit diff in other window.")

(defconst al/magit-moving-keys
  '((">"   . magit-section-up)
    ("."   . magit-section-backward)
    ("e"   . magit-section-forward)
    ("M-." . magit-section-backward-sibling)
    ("M-e" . magit-section-forward-sibling))
  "Alist of auxiliary keys for moving by magit sections.")

(defconst al/magit-keys
  '(("<backtab>" . magit-section-cycle-global)
    ("H-SPC" . magit-diff-show-or-scroll-up)
    ("M-k" . magit-copy-section-value)
    ("u" . magit-show-commit)
    ("U" . magit-unstage)
    ("E" . magit-ediff-dwim)
    ("C" . magit-cherry-pick)
    ("R" . magit-remote)
    ("1" . magit-section-show-level-1-all)
    ("2" . magit-section-show-level-2-all)
    ("3" . magit-section-show-level-3-all)
    ("4" . magit-section-show-level-4-all)
    "M-1" "M-2" "M-3" "M-4")
  "Alist of auxiliary keys for `magit-mode-map'.")

(al/bind-keys
  :map al/magit-switch-map
  ("M-m" . al/magit-switch-buffer))

(al/bind-keys-from-vars 'magit-mode-map
  '(al/lazy-scrolling-keys
    al/magit-common-keys
    al/magit-moving-keys
    al/magit-keys))

(setq
 magit-git-executable "git"
 magit-bury-buffer-function #'ignore
 magit-save-repository-buffers nil
 magit-uniquify-buffer-names nil
 magit-status-initial-section '(((unstaged) (status)) 1)

 ;; I don't use global line numbers modes anyway, so there is no need
 ;; in additional checks.
 magit-section-disable-line-numbers nil
 magit-section-initial-visibility-alist
 '((untracked . show)
   (unstaged . show)
   (unpushed . show)
   (stashes . show)))

(al/eval-after-load magit-branch
  (setq magit-branch-read-upstream-first nil)

  (transient-suffix-put 'magit-branch 'magit-branch-rename :key "R")
  (transient-suffix-put 'magit-branch 'magit-pull.rebase :key "U"))

(al/eval-after-load magit-merge
  (oset (get 'magit-merge 'transient--prefix)
        value '("--ff-only")))

(al/eval-after-load magit-tag
  (transient-suffix-put 'magit-tag 'magit-tag-create :key "n"))

(al/eval-after-load magit-log
  (put 'magit-log-mode 'magit-log-default-arguments
       '("-n99" "--decorate"))

  (transient-suffix-put 'magit-log 'magit-log:--grep :key "=g") ; grep
  (transient-suffix-put 'magit-log 'magit-log:-G :key "=p")     ; patch
  (transient-suffix-put 'magit-log 'magit:-- :key "=f")         ; file

  (defconst al/magit-log-select-keys
    '(("m" . magit-log-select-pick))
    "Alist of auxiliary keys for `magit-log-select-mode-map'.")
  (al/bind-keys-from-vars 'magit-log-mode-map
    '(al/magit-history-keys al/magit-scroll-diff-keys)
    t)
  (al/bind-keys-from-vars 'magit-log-select-mode-map
    '(al/magit-moving-keys al/magit-log-select-keys)
    t)
  (al/bind-keys-from-vars 'magit-commit-section-map
    'al/magit-common-keys
    t))

(al/eval-after-load magit-diff
  (setq-default magit-diff-refine-hunk t)
  (defconst al/magit-diff-visit-keys
    '(("u" . magit-diff-visit-worktree-file)
      ("RET" . magit-diff-visit-worktree-file)
      ("<C-return>" . magit-diff-visit-file))
    "Alist of auxiliary keys for visiting files in `magit-diff-mode'.")
  (al/bind-keys-from-vars 'magit-diff-mode-map
    'al/magit-history-keys
    t)
  (al/bind-keys-from-vars 'magit-diff-section-map
    '(al/magit-common-keys al/magit-diff-visit-keys)
    t)
  (al/bind-keys-from-vars 'magit-staged-section-map 'al/magit-common-keys)
  (al/bind-key "u" magit-section-toggle magit-file-section-map))

(al/eval-after-load magit-sequence
  (transient-suffix-put 'magit-cherry-pick "A" :key "C") ; pick
  (transient-suffix-put 'magit-rebase "u" :key "r")      ; upstream
  )

(al/eval-after-load magit-remote
  (transient-suffix-put 'magit-remote "r" :key "R") ; rename
  )

(al/eval-after-load magit-push
  (transient-suffix-put 'magit-push "p" :key "P") ; push to remote
  )

(al/eval-after-load magit-pull
  (transient-suffix-put 'magit-pull "u" :key "F") ; pull from upstream
  )

(al/eval-after-load magit-fetch
  (transient-suffix-put 'magit-fetch "u" :key "f") ; fetch from upstream
  )

(al/eval-after-load magit-blame
  (setq magit-blame-time-format "%F")
  (defconst al/magit-blame-keys
    '(("."   . magit-blame-previous-chunk)
      ("e"   . magit-blame-next-chunk)
      ("M-." . magit-blame-previous-chunk-same-commit)
      ("M-e" . magit-blame-next-chunk-same-commit)
      ("M-k" . magit-blame-copy-hash))
    "Alist of auxiliary keys for `magit-blame-mode-map'.")
  (al/bind-keys-from-vars 'magit-blame-mode-map
    '(al/lazy-scrolling-keys al/magit-blame-keys)))

(al/eval-after-load git-commit
  (al/eval-at-hook git-commit-setup-hook
    ;; Not `git-commit-turn-on-flyspell' because it calls `flyspell-buffer'.
    (flyspell-mode)
    ;; `git-commit-setup-font-lock' spoils my `text-mode' syntax stuff.
    (modify-syntax-entry ?\" "\"   ")
    (al/no-syntactic-font-lock))

  (defconst al/git-commit-keys
    '(("M->" . git-commit-prev-message)
      ("M-E" . git-commit-next-message)
      ("C-c C-a" . al/git-commit-co-authored)
      ("C-c C-r" . git-commit-reported)
      ("C-c S" . git-commit-suggested))
    "Alist of auxiliary keys for `git-commit-mode-map'.")
  (al/bind-keys-from-vars 'git-commit-mode-map 'al/git-commit-keys))

(al/eval-after-load git-rebase
  (defconst al/git-rebase-keys
    '(("p"   . git-rebase-pick)
      ("w"   . git-rebase-reword)
      ("C-k" . git-rebase-kill-line)
      ("M-." . git-rebase-move-line-up)
      ("M-e" . git-rebase-move-line-down))
    "Alist of auxiliary keys for `git-rebase-mode-map'.")
  (al/bind-keys-from-vars 'git-rebase-mode-map 'al/git-rebase-keys)

  (add-hook 'git-rebase-mode-hook #'hl-line-mode))

;;; magit.el ends here
