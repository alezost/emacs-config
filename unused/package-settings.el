;;; package-settings.el --- Old and unused settings for various packages

;; Copyright © 2012–2025 Alex Kost

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


;;; Misc

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

;;; package-settings.el ends here
