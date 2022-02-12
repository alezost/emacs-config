;;; keys.el --- Miscellaneous global key bindings and relative settings

;; Copyright © 2013–2022 Alex Kost

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

(defun al/frame-keys-actions (&optional frame)
  "Configure key bindings specific to a FRAME type."
  (keyboard-translate ?\C-x ?\C-t)
  (keyboard-translate ?\C-t ?\C-x)
  (when (display-graphic-p)
    ;; This is not possible because the index of "C-M-m" character is
    ;; too big for the char-table.
    ;;
    ;; (keyboard-translate ?\M-\r 'ctrl-alt-m)

    (keyboard-translate ?\C-m 'ctrl-m)
    (keyboard-translate ?\C-i 'ctrl-i)))

(al/add-hook-maybe
    '(after-make-frame-functions window-setup-hook)
  'al/frame-keys-actions)


;;; Keys for multiple maps

(defconst al/free-moving-keys
  '("C-o" "M-o" "C-M-o" "M-O"
    "C-u" "M-u" "C-M-u" "M-U"
    "C-." "M-." "C-M-." "M->"
    "C-e" "M-e" "C-M-e" "M-E"
    "C-a" "M-a" "C-M-a" "M-A"
    "<ctrl-i>" "M-i" "C-M-i" "M-I")
  "Alist of moving keys that should be unbound.")

(defconst al/free-editing-keys
  '("C-," "M-," "C-M-," "M-<"
    "C-p" "M-p" "C-M-p" "M-P"
                "C-M-q" "M-Q"
    "C-k" "M-k" "C-M-k" "M-K"
    "C-'" "M-'" "C-M-'" "M-\""
    "C-;" "M-;" "C-M-;"
    "C-t" "M-x" "C-M-x")
  "Alist of editing keys that should be unbound.")

(defconst al/free-important-keys
  '("M-g")
  "Alist of important keys that should be unbound.")

(defconst al/free-misc-keys
  '("M-_" "M-+"
    "M-/" "C-M-/" "M-?"
    "C-w"
    "C-M-t" "<C-tab>" "<M-tab>" "<C-M-tab>")
  "Alist of miscellaneous keys that should be unbound.")

(defconst al/lazy-moving-keys
  '(("o" . backward-char)
    ("u" . forward-char)
    ("." . previous-line)
    ("e" . next-line))
  "Alist of auxiliary keys for lazy moving.")

(defconst al/lazy-scrolling-keys
  '(("SPC" . scroll-up-command)
    ("DEL" . scroll-down-command))
  "Alist of auxiliary keys for lazy scrolling.")

(defconst al/button-keys
  '(("." . backward-button)
    ("e" . forward-button)
    ("u" . push-button))
  "Alist of auxiliary keys for modes with buttons.")

(defconst al/minibuffer-keys
  '("C-j" ; to insert newlines during evaluating expressions
    ("M-." . previous-history-element)
    ("M-e" . next-history-element))
  "Alist of auxiliary keys for minibuffer modes.")

(setq al/default-keys-variables
      '(al/free-moving-keys al/free-editing-keys al/free-important-keys))


;;; Hydra

(with-eval-after-load 'hydra
  (setq hydra-verbose t)
  (al/bind-keys
   :map hydra-base-map
   ("C-4" . hydra--universal-argument)
   ("C-u"))
  (hydra-add-font-lock))


;;; Global keys

(al/bind-keys
 :map ctl-x-map
 ("A"     (apply #'al/update-autoloads
                 (if current-prefix-arg
                     (al/subdirs al/emacs-my-packages-dir)
                   (list al/emacs-utils-dir))))
 ("C"   . save-buffers-kill-emacs)
 ("C-8" . insert-char))

(al/bind-keys
 :map universal-argument-map
 ("C-4" . universal-argument-more)
 ("C-u"))

(al/bind-keys
 ("C-4"         . universal-argument)

 ("H-u"         . undo)
 ("H-M-u"       . undo-only)

 ("C-M-c"       . calc)

 ("H-m H-m"     . kmacro-end-or-call-macro)
 ("<f4>"        . kmacro-end-or-call-macro)
 ("<XF86New>"   . kmacro-end-or-call-macro)
 ("H-m s"       . kmacro-start-macro-or-insert-counter)
 ("H-m RET"     . kmacro-start-macro-or-insert-counter)
 ("<C-f4>"      . kmacro-start-macro-or-insert-counter)
 ("<C-XF86New>" . kmacro-start-macro-or-insert-counter)
 ("H-m e"       . kmacro-edit-macro)
 ("H-m C-d"     . kmacro-edit-macro)
 ("<M-f4>"      . kmacro-edit-macro)
 ("<M-XF86New>" . kmacro-edit-macro)
 ("H-m A"         (kmacro-call-macro 0))

 ("<C-kp-add>"      . text-scale-increase)
 ("<C-kp-subtract>" . text-scale-decrease)
 ("<C-kp-multiply>"   (text-scale-set 0))

 ("<f5>"        . compile)
 ("C-="         . describe-char)
 ("C-c x"       . exit-recursive-edit)
 ("C-c r"       . revert-buffer)
 ("C-c p"       . list-processes)
 ("C-c e"       . list-environment)
 ("C-c k"       . al/kill-process))

(defalias 'ctl-x-r-prefix ctl-x-r-map)
(al/bind-key "M-R" ctl-x-r-prefix)
(al/bind-keys
 :map ctl-x-r-map
 ("a" . append-to-register)
 ("p" . prepend-to-register))

(defalias 'goto-prefix goto-map)
(al/bind-key "C-M-g" goto-prefix)
(al/bind-keys
 :map goto-map
 ("C-M-g" . goto-line)
 ("c"     . move-to-column)
 ("p"     . goto-char)
 ("h"     . previous-error)
 ("C-M-h" . previous-error)
 ("C-M-n" . next-error))

(al/bind-keys
 :prefix-map al/modes-map
 :prefix-docstring "Map for enabling/disabling modes."
 :prefix "M-M"
 ("M-M" . al/major-mode-to-kill-ring)
 ("a" . artist-mode)
 ("A" . auto-fill-mode)
 ("c" . conf-unix-mode)
 ("e" . emacs-lisp-mode)
 ("f" . font-lock-mode)
 ("o" . org-mode)
 ("p" . python-mode)
 ("r" . rainbow-mode)
 ("l" . nlinum-mode)
 ("s" . sh-mode)
 ("t" . toggle-truncate-lines)
 ("v" . view-mode)
 ("T" . text-mode)
 ("|" . indent-guide-mode))

;;; keys.el ends here
