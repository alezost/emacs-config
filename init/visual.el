;;; visual.el --- Visual settings: fonts, themes, mode-line, …  -*- lexical-binding: t -*-

;; Copyright © 2012–2018 Alex Kost

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

(require 'al-key)


;;; Frame specific settings

(defun al/frame-visual-actions (&optional frame)
  "Perform some visual actions specific to a FRAME type."
  (when (and (display-graphic-p)
             (require 'al-font nil t))
    (set-frame-font (al/first-existing-font) nil t)
    ;; Should be "solved": 武; 🐼, 😻, ⚽, 💩, ∵, ⸪ (symbola);
    ;; ࿌ (unifont); 🃜, 🜒, 🝖 (quivira).
    (al/set-fontset
     "fontset-default" nil nil
     '(("Symbola"
        (#x2020  . #x24ff)
        (#x2600  . #x27ff)
        (#x2900  . #x29ff)
        (#x2e00  . #x2e42)
        (#x1d300 . #x1d371)
        (#x1d400 . #x1d7ff)
        (#x1f000 . #x1f1ff)
        (#x1f300 . #x1f9ff))
       ("Ubuntu Mono"
        (?²      . ?³)
        (?¼      . ?¾)
        (#x2070  . #x208f))
       ("Quivira" nil)))))

(al/add-hook-maybe
    '(after-make-frame-functions window-setup-hook)
  'al/frame-visual-actions)


;;; Global keys

(al/bind-keys
 :prefix-map al/visual-map
 :prefix-docstring "Map for visual stuff."
 :prefix "M-V"
 ("T"   . tool-bar-mode)
 ("M"   . menu-bar-mode)
 ("S"   . scroll-bar-mode)
 ("I"   . tooltip-mode)
 ("r"   . rainbow-mode)
 ("t"   . al/load-theme)
 ("C"   . make-color)
 ("c"   . make-color-switch-to-buffer)
 ("l"     (al/load-theme 'alect-light))
 ("M-l"   (al/load-theme 'alect-light-alt))
 ("d"     (al/load-theme 'alect-dark))
 ("M-d"   (al/load-theme 'alect-dark-alt))
 ("b"     (al/load-theme 'alect-black))
 ("M-b"   (al/load-theme 'alect-black-alt))
 ("h"   . hl-line-mode)
 ("w"   . whitespace-mode)
 ("W"   . global-whitespace-mode)
 ("M-W"   (setq show-trailing-whitespace
                (not show-trailing-whitespace)))
 ("f"   . al/face-to-kill-ring)
 ("F"   . facemenu-set-foreground)
 ("B"   . facemenu-set-background)
 ("M-F" . make-color-foreground-color-to-kill-ring)
 ("M-B" . make-color-background-color-to-kill-ring))


;;; Themes

(with-eval-after-load 'custom
  (setq custom-safe-themes t)

  ;; Fix bug <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16266>.
  (defun al/fix-custom-variables-bug (fun &rest args)
    "Allow setting undefined variables in themes."
    (let (custom--inhibit-theme-enable)
      (apply fun args)))
  (advice-add 'custom-theme-set-variables
    :around #'al/fix-custom-variables-bug))

(with-eval-after-load 'alect-themes
  (setq
   alect-display-class '((class color) (min-colors 256))
   alect-overriding-faces
   '((hl-line ((((type graphic)) :background bg)
               (t :background unspecified)))))
  (when al/emacs-trunk?
    (push '(fringe ((((class color) (min-colors 256))
                     :foreground gray :background bg+2)))
          alect-overriding-faces)))

(al/eval-after-init
  (and (require 'alect-themes nil t)
       (require 'al-color nil t)
       (al/load-theme 'alect-light)))


;;; Mode line

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
    (:eval (list (propertize " " 'display
                             `(space :align-to (- right ,(length server-name))))
                 server-name)))
  "Mode line construct for displaying `server-name' if server is running.")
(put 'al/mode-server 'risky-local-variable t)

(when (require 'dim nil t)
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
     (indent-guide-mode          " ¦"           indent-guide)
     (rainbow-mode               " 🖌"           rainbow-mode)
     (abbrev-mode                " Ab"          abbrev)
     (company-mode               " ⍈"           company)
     (yas-minor-mode             " ⮞"           yasnippet)
     (paredit-mode               " PE"          paredit)
     (view-mode                  " 👀"           view)
     (eldoc-mode                 ""             eldoc)
     (edebug-mode                " 🔧"           edebug)
     (counsel-mode               ""             counsel)

     (gnus-topic-mode            " T"           gnus-topic)
     (gnus-dired-mode            " 𝗚"           gnus-dired)

     (guix-build-log-minor-mode  " γ🄻"          guix-build-log)
     (guix-devel-mode            " γ"           guix-devel)

     (magit-blame-mode           " µBlame"      magit-blame)
     (erc-notifications-mode     " 🗩"           erc-desktop-notifications)
     (al/emms-notification-mode " 🎧"           al/emms)
     (flyspell-mode              " fly"         flyspell))))

(setq-default
 mode-line-format
 `("%e" mode-line-front-space
   mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
   " " mode-line-buffer-identification " " mode-line-position
   " %l,%c"
   (vc-mode vc-mode)
   " " mode-line-modes mode-line-misc-info
   al/mode-server
   mode-line-end-spaces)

 mode-line-buffer-identification
 (propertized-buffer-identification "%b")

 mode-line-mule-info
 `(""
   (current-input-method
    (:propertize
     current-input-method-title
     help-echo (concat ,(purecopy "Input method: ")
                       current-input-method
                       ,(purecopy
                         (concat "\n" "mouse-2: Disable input method\n"
                                 "mouse-3: Describe input method")))
     local-map ,mode-line-input-method-map
     face font-lock-warning-face
     mouse-face mode-line-highlight))
   ,(propertize
     "%z"
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
   (list '(:eval
           (let ((proc (get-buffer-process (current-buffer))))
             (propertize
              (if proc (symbol-name (process-status proc)) "–")
              'face 'font-lock-constant-face)))
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
            (:propertize al/mode-info face font-lock-comment-face)
            ")"))
         `(:propertize minor-mode-alist
                       mouse-face mode-line-highlight
                       help-echo ,mode-help-echo
                       local-map ,mode-line-minor-mode-keymap)
         '(:eval
           (if (buffer-narrowed-p)
               (propertize " ↕"
                           'help-echo "mouse-1: Remove narrowing"
                           'mouse-face 'mode-line-highlight
                           'local-map (make-mode-line-mouse-map 'mouse-1 #'mode-line-widen))
             ""))
         "│"
         (propertize "%]"
                     'help-echo recursive-edit-help-echo
                     'face 'font-lock-warning-face))))


;;; Misc settings and packages

(setq-default
 indicate-buffer-boundaries 'left
 visual-line-fringe-indicators '(nil vertical-bar)
 indicate-empty-lines t
 font-lock-extra-managed-props '(composition))

(setq jit-lock-defer-time 0.1)

;; Make page breaks look fancier than the default "^L".
;; Idea from <http://www.jurta.org/en/emacs/dotemacs>.
(or standard-display-table
    (setq standard-display-table (make-display-table)))
(aset standard-display-table ?\^L
      (let ((line (make-vector 30 ?—)))
        (vconcat line " page break " line)))

(column-number-mode)
(blink-cursor-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
;; (mouse-avoidance-mode 'banish)

(setq tooltip-delay 0.2)
(setq scroll-bar-mode 'right)
(scroll-bar-mode 0)

(with-eval-after-load 'whitespace
  (setq
   whitespace-line-column 78
   whitespace-display-mappings
   `((space-mark   ?\s  [?·])
     (space-mark   ?    [?○])
     ;; (newline-mark ?\n  [?↵ ?\n])
     (newline-mark ?\^L ,(aref standard-display-table ?\^L))
     (tab-mark     ?\t  [?⇉ ?\t]))
   whitespace-style
   '(face spaces tabs trailing lines space-before-tab newline
          indentation space-after-tab tab-mark newline-mark)))

(with-eval-after-load 'ruler-mode
  (setq ruler-mode-show-tab-stops t))

(setq show-paren-delay 0.1)
(with-eval-after-load 'paren
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))
(al/add-after-init-hook 'show-paren-mode)

(with-eval-after-load 'indent-guide
  (setq
   indent-guide-delay 0.3
   indent-guide-char "¦")
  ;; https://github.com/zk-phi/indent-guide/issues/29
  (defun al/indent-guide-post-command-hook ()
    (if (null indent-guide-delay)
        (indent-guide-show)
      (run-with-idle-timer indent-guide-delay nil
                           #'indent-guide-show)))
  (advice-add 'indent-guide-post-command-hook
    :override 'al/indent-guide-post-command-hook))

(with-eval-after-load 'make-color
  (al/add-hook-maybe 'make-color-mode-hook 'al/bar-cursor-type))

(with-eval-after-load 'rainbow-mode
  (setq rainbow-x-colors t)
  (advice-add 'rainbow-mode :after #'al/refontify))

(setq hl-todo-keyword-faces
      (mapcar (lambda (word)
                (cons word 'hl-todo))
              '("TODO" "FIXME" "XXX" "WARNING" "ERROR"))
      hl-todo-keywords
      `(((lambda (_)
           (let (case-fold-search)
             (re-search-forward hl-todo-regexp nil t)))
         (1 (hl-todo-get-face) t t))))

;;; visual.el ends here
