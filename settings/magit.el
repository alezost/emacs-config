;;; magit.el --- Settings for `magit' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'magit)
(require 'al-key)
(require 'al-magit)

(al/bind-keys
  :map al/magit-switch-map
  ("M-m" 'al/magit-switch-buffer))

(al/bind-keys
  :map al/magit-common-map
  :create t
  ("v" 'magit-git-command)
  ;; XXXneeded?
  ;; "M-m"
  )

(al/bind-keys
  :map al/magit-history-map
  :create t
  ("↷" 'magit-go-backward)
  ("↶" 'magit-go-forward))

(al/bind-keys
  :map magit-section-mode-map
  ([tab] 'magit-section-toggle)
  "M-1" "M-2" "M-3" "M-4"
  ("1" 'magit-section-show-level-1-all)
  ("2" 'magit-section-show-level-2-all)
  ("3" 'magit-section-show-level-3-all)
  ("4" 'magit-section-show-level-4-all))

(defvar al/lazy-scrolling-map)

(al/bind-keys
  :map magit-mode-map
  :parent (al/lazy-scrolling-map
           al/magit-common-map
           magit-section-mode-map)
  ("S-↑" 'magit-section-up)
  ("↑"   'magit-section-backward)
  ("↓"   'magit-section-forward)
  ("M-↑" 'magit-section-backward-sibling)
  ("M-↓" 'magit-section-forward-sibling)
  ("<backtab>" 'magit-section-cycle-global)
  ("H-SPC" 'magit-diff-show-or-scroll-up)
  ("M-k" 'magit-copy-section-value)
  ("→"   'magit-show-commit)
  ("U"   'magit-unstage)
  ("E"   'magit-ediff-dwim)
  ("C"   'magit-cherry-pick)
  ("R"   'magit-remote))

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

  (al/bind-keys
    :map magit-log-mode-map
    :parent (al/magit-history-map magit-mode-map)
    ;; XXX check: not needed (inherited from `magit-mode-map')
    ;; ("SPC" 'magit-diff-show-or-scroll-up)
    ;; ("DEL" 'magit-diff-show-or-scroll-down)
    )
  (al/bind-keys
    :map magit-log-select-mode-map
    ("m" 'magit-log-select-pick))
  (al/bind-keys
    :map magit-commit-section-map
    :parent al/magit-common-map))

(al/eval-after-load magit-diff
  (setq-default magit-diff-refine-hunk t)

  (al/bind-keys
    :map magit-diff-mode-map
    :parent (al/magit-history-map magit-mode-map))

  (al/bind-keys
    :map magit-diff-section-map
    :parent al/magit-common-map
    ("→" 'magit-diff-visit-worktree-file)
    ("RET" 'magit-diff-visit-worktree-file)
    ("<C-return>" 'magit-diff-visit-file))

  (al/bind-keys
    :map magit-staged-section-map
    :parent al/magit-common-map)

  (al/bind-keys
    :map magit-file-section-map
    ("→" 'magit-section-toggle)))

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

  (al/bind-keys
    :map magit-blame-mode-map
    :parent al/lazy-scrolling-map
    ("↑"   'magit-blame-previous-chunk)
    ("↓"   'magit-blame-next-chunk)
    ("M-↑" 'magit-blame-previous-chunk-same-commit)
    ("M-↓" 'magit-blame-next-chunk-same-commit)
    ("M-k" 'magit-blame-copy-hash)))

(al/eval-after-load git-commit
  (al/bind-keys
    :map git-commit-mode-map
    ("M-S-↑" 'git-commit-prev-message)
    ("M-S-↓" 'git-commit-next-message)
    ("C-c C-a" 'al/git-commit-co-authored)
    ("C-c C-r" 'git-commit-reported)
    ("C-c S" 'git-commit-suggested))

  (al/eval-at-hook git-commit-setup-hook
    ;; Not `git-commit-turn-on-flyspell' because it calls `flyspell-buffer'.
    (flyspell-mode)
    ;; `git-commit-setup-font-lock' spoils my `text-mode' syntax stuff.
    (modify-syntax-entry ?\" "\"   ")
    (al/no-syntactic-font-lock)))

(al/eval-after-load git-rebase
  (al/bind-keys
    :map git-rebase-mode-map
    ("p"   'git-rebase-pick)
    ("w"   'git-rebase-reword)
    ("C-k" 'git-rebase-kill-line)
    ("M-↑" 'git-rebase-move-line-up)
    ("M-↓" 'git-rebase-move-line-down))

  (add-hook 'git-rebase-mode-hook #'hl-line-mode))

;;; magit.el ends here
