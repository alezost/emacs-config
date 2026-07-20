;;; package-settings.el --- Old and unused settings for various packages

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


;;; Working with text

(with-eval-after-load 'paredit
  (al/clean-map 'paredit-mode-map)
  (al/bind-keys
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
   ("H-E"       . paredit-splice-sexp)
   ("H-P"       . paredit-splice-sexp-killing-backward)
   ("H-<"       . paredit-splice-sexp-killing-forward)
   ("H->"       . paredit-raise-sexp)
   ("C-)"       . paredit-forward-slurp-sexp)
   ("C-M-0"     . paredit-forward-barf-sexp)
   ("C-("       . paredit-backward-slurp-sexp)
   ("C-M-9"     . paredit-backward-barf-sexp)
   ("C-M-5"     . paredit-split-sexp)
   ("C-M-6"     . paredit-join-sexps))
  (add-to-list 'emulation-mode-map-alists
               `((paredit-mode . ,paredit-mode-map))))

(with-eval-after-load 'smartparens
  (al/bind-keys
   :map smartparens-mode-map
   ("<H-M-tab>" . sp-indent-defun)
   ("M-p"       . sp-backward-kill-word)
   ("M-,"       . sp-kill-word)
   ("C-M-o"     . sp-backward-sexp)
   ("C-M-u"     . sp-forward-sexp)
   ("C-M-."     . sp-backward-up-sexp)
   ("C-M-e"     . sp-down-sexp)
   ("H-E"       . sp-splice-sexp)
   ("H-<"       . sp-splice-sexp-killing-forward)
   ("H-P"       . sp-splice-sexp-killing-backward)
   ("H->"       . sp-splice-sexp-killing-around)
   ("C-M-p"     . sp-backward-kill-sexp)
   ("C-M-k"     . sp-backward-copy-sexp)
   ("C-M-,"     . sp-kill-sexp)
   ("C-M-q"     . sp-copy-sexp)
   ("C-M-'"     . sp-transpose-sexp)
   ("C-)"       . sp-forward-slurp-sexp)
   ("C-M-0"     . sp-forward-barf-sexp)
   ("C-("       . sp-backward-slurp-sexp)
   ("C-M-9"     . sp-backward-barf-sexp))
  (al/bind-keys
   :map smartparens-mode-map
   :prefix-map al/smartparens-map
   :prefix-docstring "Map for misc smartparens commands."
   :prefix "H-p"
   ("c" . sp-cheat-sheet)
   ("." . sp-absorb-sexp)
   ("e" . sp-emit-sexp)
   ("o" . sp-convolute-sexp)
   ("j" . sp-join-sexp)
   ("s" . sp-split-sexp)))


;;; Completion engines

(with-eval-after-load 'ido
  (setq
   ;; Disable auto searching for files unless called explicitly.
   ido-auto-merge-delay-time 999
   ido-enable-last-directory-history t
   ido-save-directory-list-file (al/emacs-data-dir-file "ido.last")
   ido-record-commands nil
   ido-enable-tramp-completion nil
   ido-enable-flex-matching t
   ido-create-new-buffer 'always
   ido-decorations
   '("\n ● " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]"
     " [Not readable]" " [Too big]" " [Confirm]" "\n ● " " ●"))

  (defconst al/ido-common-keys
    '(("C-l"    . ido-toggle-ignore)
      ("C-M-l"  . ido-toggle-regexp)
      ("C-."    . ido-prev-match)
      ("C-e"    . ido-next-match)
      ("<up>"   . ido-prev-match)
      ("<down>" . ido-next-match)
      ("C-d"    . ido-fallback-command)
      ("M-d"    . ido-edit-input)
      ("M-k"    . al/ido-copy-current-item)
      ("M-s"    . ido-select-text)
      ;; C-j is unbound in `minibuffer-local-map'
      ("C-j"    . ido-select-text)
      "SPC")
    "Alist of auxiliary keys for `ido-common-completion-map'.")
  (defconst al/ido-file-dir-keys
    '(("H-j"   . ido-enter-dired)
      ("M-."   . ido-prev-work-directory)
      ("M-e"   . ido-next-work-directory)
      ("C-M-." . ido-prev-match-dir)
      ("C-M-e" . ido-next-match-dir)
      ("M-m"   . ido-enter-magit-status)  ; in external `magit-ido' package
      ("M-h"     (al/ido-set-current-directory "~"))
      ("M-g"     (al/ido-set-current-directory al/guix-profile-dir)))
    "Alist of auxiliary keys for `ido-file-dir-completion-map'.")
  (al/bind-keys-from-vars
      '(ido-common-completion-map
        ido-buffer-completion-map)
    '(al/minibuffer-keys al/ido-common-keys))
  (al/bind-keys-from-vars
      '(ido-file-dir-completion-map
        ido-file-completion-map)
    '(al/ido-file-dir-keys al/ido-common-keys))

  (when (require 'al-ido nil t)
    (advice-add 'ido-completions :override #'al/ido-completions))

  (al/add-hook-maybe 'ido-minibuffer-setup-hook 'al/no-truncate-lines)

  (ido-everywhere))

(with-eval-after-load 'smex
  (setq
   smex-save-file (al/emacs-data-dir-file "smex-items")
   smex-history-length 32
   smex-prompt-string
   (concat (key-description (where-is-internal 'smex nil t))
           " (smex): "))
  (defun al/smex-prepare-ido-bindings ()
    "Add my bindings to the pseudo smex map."
    (let ((map ido-completion-map))
      (define-key map (kbd "C-h f") 'smex-describe-function)
      (define-key map (kbd "C-h w") 'smex-where-is)
      (define-key map (kbd "M-d")   'smex-find-function)
      (define-key map (kbd "C-d")   'smex-describe-function)))
  (advice-add 'smex-prepare-ido-bindings
    :override 'al/smex-prepare-ido-bindings))

(with-eval-after-load 'ivy
  (setq
   ;; Since I don't use `ivy-mode' (as it sets
   ;; `completing-read-function'), set `completion-in-region-function'
   ;; manually.
   completion-in-region-function 'ivy-completion-in-region
   ;; Do not exit from minibuffer when there is nothing to delete.
   ivy-on-del-error-function 'ignore
   ivy-initial-inputs-alist nil
   ivy-sort-functions-alist nil
   ivy-sort-matches-functions-alist '((t . nil))
   ivy-sort-max-size 1000
   ivy-use-virtual-buffers t
   ivy-re-builders-alist '((t . ivy--regex-fuzzy))
   ivy-wrap t
   ivy-extra-directories nil)

  (defconst al/ivy-minibuffer-keys
    '(("TAB" . al/ivy-partial)
      ("RET" . ivy-alt-done)
      ("C-j" . ivy-immediate-done)
      ("C-l" . ivy-toggle-ignore)
      ("M-." . ivy-previous-history-element)
      ("M-e" . ivy-next-history-element)
      ("M-k" . al/ivy-copy-current-item))
    "Alist of auxiliary keys for `ivy-minibuffer-map'.")
  (al/bind-keys-from-vars 'ivy-minibuffer-map 'al/ivy-minibuffer-keys)

  (when (require 'al-ivy nil t)
    (setq ivy-format-function 'al/ivy-format-function)
    (push '(imenus . al/ivy-imenu-sort)
          ivy-sort-matches-functions-alist)
    (advice-add 'ivy-add-prompt-count
      :override 'al/ivy-add-prompt-count)))

(with-eval-after-load 'counsel
  (define-key counsel-mode-map [remap switch-to-buffer]
    'ivy-switch-buffer)

  (defconst al/counsel-describe-keys
    '(("M-d" . counsel-find-symbol))
    "Alist of auxiliary keys for `counsel-describe-map'.")
  (al/bind-keys-from-vars 'counsel-describe-map
    'al/counsel-describe-keys)

  (defconst al/counsel-find-file-keys
    '(("M-h"   (ivy--cd "~/"))
      ("M-m" . al/ivy-magit-status))
    "Alist of auxiliary keys for `counsel-find-file-map'.")
  (al/bind-keys-from-vars 'counsel-find-file-map
    'al/counsel-find-file-keys)

  (when (require 'al-file nil t)
    (setq counsel-find-file-ignore-regexp
          (al/file-regexp "elc" "go"))))

(with-eval-after-load 'vertico
  (setq vertico-cycle t)

  (defconst al/vertico-keys
    '([remap exit-minibuffer]
      ("C-j" . exit-minibuffer)
      ("RET" . vertico-directory-enter)
      ("DEL" . vertico-directory-up)
      ("M-h"   (al/minibuffer-set-directory "~")))
    "Alist of auxiliary keys for `vertico-map'.")
  (al/bind-keys-from-vars 'vertico-map 'al/vertico-keys))


;;; SLIME

;; This should be set before loading slime.
(al/setq-no-warnings
 slime-contribs
 '(slime-repl
   slime-autodoc
   ;; slime-editing-commands  ; Binds C-M-e to some rubbish
   ;; slime-c-p-c             ; requires slime-editing-commands
   slime-fancy-inspector
   slime-fancy-trace
   slime-fuzzy
   slime-mdot-fu
   slime-macrostep
   slime-presentations
   ;; slime-scratch
   slime-references
   slime-package-fu
   slime-fontifying-fu
   slime-trace-dialog
   slime-indentation))

(al/eval-after-init
  ;; Use SLIME from quicklisp.
  (let* ((quicklisp-dir  (expand-file-name "~/.quicklisp"))
         (swank.txt-file (expand-file-name
                          "dists/quicklisp/installed/systems/swank.txt"
                          quicklisp-dir)))
    (al/with-check
      :file swank.txt-file
      (let* ((swank.txt (with-temp-buffer
                          (insert-file-contents swank.txt-file)
                          (buffer-string)))
             (slime-dir (file-name-directory
                         (expand-file-name swank.txt quicklisp-dir))))
        (al/add-to-load-path-maybe slime-dir)
        (al/autoload "slime" slime slime-mode slime-lisp-mode-hook)
        (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)))))

;; `al/slime-keys' is required for `al/erc-channel-config'
(defconst al/slime-keys
  '(("C-v"     . al/slime-eval-dwim)
    ("C-M-v"   . slime-eval-defun)
    ("M-s-v"   . slime-eval-buffer)
    ("C-S-v"   . slime-expand-1)
    ("C-d"     . slime-describe-symbol)
    ("M-d"     . slime-edit-definition)
    ("C-M-d"   . slime-doc-map)
    "C-c C-d")
  "Alist of auxiliary keys for slime modes.")
(al/bind-keys
 :prefix-map al/slime-map
 :prefix-docstring "Map for slime commands."
 :prefix "M-L"
 ("l"   . slime-repl)
 ("M-L" . slime-repl)
 ("c"   . al/slime-stumpwm-connect)
 ("d"   . slime-disconnect)
 ("M-S" . slime)
 ("s"   . slime-selector))

(al/eval-after-load slime
  (setq
   inferior-lisp-program "sbcl"
   ;; slime-lisp-implementations
   ;; `((sbcl ("sbcl" "--core" ,(al/src-dir-file "sbcl-with-swank"))))
   ;; Do not ask about version difference.
   slime-protocol-version 'ignore)

  (defconst al/slime-xref-keys
    '(("." . slime-xref-prev-line)
      ("e" . slime-xref-next-line)
      ("u" . slime-goto-xref)
      ("d" . slime-show-xref))
    "Alist of auxiliary keys for `slime-xref-mode'.")
  (al/bind-keys-from-vars 'slime-xref-mode-map 'al/slime-xref-keys)

  (defconst al/slime-doc-keys
    '(("C-d" . slime-documentation-lookup))
    "Alist of auxiliary keys for `slime-doc-map'.")
  (al/bind-keys-from-vars 'slime-doc-map 'al/slime-doc-keys)

  (al/bind-keys-from-vars 'slime-parent-map
    '(al/free-misc-keys al/slime-keys))
  (al/bind-keys-from-vars '(slime-mode-map slime-editing-map)))

(al/eval-after-load slime-repl
  ;; "C-c C-j" (in `slime-mode-map') is bound in "slime-repl.el", so
  ;; override it here.
  (al/bind-key "C-c C-j"
    al/slime-switch-to-repl-and-enter
    slime-mode-map)

  (defconst al/slime-repl-keys
    '(("M-." . slime-repl-previous-input)
      ("M-e" . slime-repl-next-input)
      ("M->" . slime-repl-previous-prompt)
      ("M-E" . slime-repl-next-prompt)
      ("M-r" . slime-repl-previous-matching-input))
    "Alist of auxiliary keys for `slime-repl-mode-map'.")
  (al/bind-keys-from-vars 'slime-repl-mode-map 'al/slime-repl-keys))

(al/eval-after-load slime-autodoc
  ;; `slime-autodoc-mode' binds some useless keys into "C-c C-d" prefix.
  (al/clean-map 'slime-autodoc-mode-map)
  (al/bind-keys
   :map slime-autodoc-mode-map
   ("SPC" . slime-autodoc-space)))


;;; Windows and frames

(setq
 winner-dont-bind-my-keys t
 winner-ring-size 40)
(al/bind-keys
 ("<C-left>"  . winner-undo)
 ("<C-right>" . winner-redo))
(al/add-after-init-hook 'winner-mode)


;;; Misc

(with-eval-after-load 'org-emms
  (setq org-emms-delay 2
        org-emms-time-format "%m:%.2s")
  (when (require 'al-emms-mpv nil t)
    (defun al/org-emms-sync-time (&rest _)
      ;; This is asynchronous, so we need to wait.
      (al/emms-mpv-sync-playing-time)
      (sleep-for 1))
    (advice-add 'org-emms-make-link :before #'al/org-emms-sync-time)))

(al/bind-keys
 :prefix-map al/echo-msk-map
 :prefix-docstring "Map for echo-msk."
 :prefix "C-M-s-e"
 ("p" . echo-msk-program-task)
 ("s" . echo-msk-browse-schedule)
 ("a" . echo-msk-emms-play-online-audio)
 ("A" . echo-msk-browse-online-audio)
 ("v" . echo-msk-browse-online-video))

(with-eval-after-load 'echo-msk
  (when (require 'dvorak-russian-computer nil t)
    (setq echo-msk-input-method "dvorak-russian-computer")))

(al/bind-keys
 ("C-c s" . al/sauron-toggle-hide-show)
 ("C-c S" . al/sauron-restart))

(with-eval-after-load 'sauron
  (setq
   sauron-max-line-length 174
   sauron-separate-frame nil
   sauron-modules nil
   sauron-nick-insensitivity 10
   sauron-scroll-to-bottom nil)
  (setq sauron-watch-patterns
        (append sauron-watch-patterns
                '("theme" "color" "debpaste" "guix\\.el"
                  "game" "ducpel" "sokoban")))
  (add-to-list 'sauron-modules 'sauron-erc))

(with-eval-after-load 'sunrise-commander
  (setq
   sr-listing-switches "-alh --group-directories-first --no-group"
   sr-show-hidden-files nil
   sr-confirm-kill-viewer nil
   sr-modeline-use-utf8-marks t)
  ;; Do not block windows resizing with `sr-lock-window'.
  (remove-hook 'window-size-change-functions 'sr-lock-window)

  (defconst al/sr-keys
    '(("i"   . sr-show-files-info)
      ("o"   . sr-dired-prev-subdir)
      ("u"   . sr-advertised-find-file)
      ("M-u" . sr-advertised-find-file-other)
      (","   . sr-history-prev)
      ("p"   . sr-history-next)
      ("y"   . sr-synchronize-panes)
      ("H-a" . sr-beginning-of-buffer)
      ("H-i" . sr-end-of-buffer)
      ("V"     (sr-quick-view t)))
    "Alist of auxiliary keys for `sr-mode-map'.")
  (al/bind-keys-from-vars 'sr-mode-map 'al/sr-keys))

(with-eval-after-load 'hydra
  (setq hydra-verbose t)
  (al/bind-keys
   :map hydra-base-map
   ("C-4" . hydra--universal-argument)
   ("C-u"))
  (hydra-add-font-lock))

(al/bind-keys
 :prefix-map al/tags-map
 :prefix-docstring "Map for tags."
 :prefix "M-T"
 ("M-T" . find-tag)
 ("d"     (find-tag (find-tag-default)))
 ("r"   . find-tag-regexp)
 ("n"   . tags-loop-continue)
 ("v"   . visit-tags-table)
 ("c"   . al/create-tags))

(al/autoload "dictem"
  dictem-run-search
  dictem-run-match)

(al/with-eval-after-load dictem
  ;; Load "dictem-db.el" where I put `dictem-database-alist' and
  ;; `dictem-strategy-alist' generated by "M-x dictem-initialize".
  (al/load (al/emacs-data-dir-file "dictem-db"))
  (unless (and dictem-database-alist
               dictem-strategy-alist)
    (dictem-initialize))
  (setq dictem-use-existing-buffer nil)

  (al/bind-keys
    :map dictem-mode-map
    ("." . dictem-previous-link)
    ("e" . dictem-next-link)
    ("u" . dictem-define-on-press)
    ("h" . dictem-previous-section)
    ("n" . dictem-next-section)
    ("m" . dictem-hyperlinks-menu)
    ("M" . dictem-run-match)
    ("Q" . dictem-kill-all-buffers))

  (add-hook 'dictem-postprocess-match-hook
            #'dictem-postprocess-match)
  (add-hook 'dictem-postprocess-definition-hook
            #'dictem-postprocess-definition-separator)
  (add-hook 'dictem-postprocess-definition-hook
            #'dictem-postprocess-definition-hyperlinks)
  (add-hook 'dictem-postprocess-show-info-hook
            #'dictem-postprocess-definition-hyperlinks)

  (al/require al-dictem))

(al/with-eval-after-load al-dictem
  (setq al/dictem-dicts
        '(nil "mueller7" "korolew_en-ru" "korolew_ru-en"
              "slovnyk_ru-en" "ushakov" "fd-eng-lat" "fd-lat-eng"))
  (advice-add 'dictem :override #'al/dictem)
  (advice-add 'dictem-define-on-press
    :override #'al/dictem-define-on-press))

;;; package-settings.el ends here
