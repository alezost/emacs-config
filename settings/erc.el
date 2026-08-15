;;; erc.el --- Settings for `erc' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'erc)
(require 'erc-join)
(require 'erc-log)
(require 'al-key)
(require 'al-erc)

(defconst al/erc-keys
  '("TAB"
    ("M-." . erc-previous-command)
    ("M-e" . erc-next-command)
    ("C-a" . erc-bol)
    ("C-c C-d" . al/erc-part-or-quit)
    ("C-l" . al/erc-view-log-file)
    ("<s-kanji>" . al/recenter-end-of-buffer-top)
    ("C-H-3" . al/recenter-end-of-buffer-top)))

;; Not binding at the top level because some modules (`erc-ring') add
;; their key bindings to `erc-mode-map'.
(al/eval-at-hook erc-ring-mode-hook
  (al/bind-keys-from-vars 'erc-mode-map 'al/erc-keys))

(setq
 erc-autojoin-channels-alist
 '(("libera"
    "#emacs"
    "#erc"
    "#gnus"
    "#scheme"
    "#guile"
    "#guix"
    "#geiser"
    "#dunst"
    "#lisp"
    "#nyxt"
    "#stumpwm"
    "#openmw"
    "#yt-dlp"
    "#mpv"
    "##math"
    "#ai"
    "#org-mode"))
 al/erc-channel-list
 (append (cdar erc-autojoin-channels-alist)
         '("#archlinux"
           "##programming"
           "##English"
           "##latin"
           "#lispgames"
           "#git"
           "#guix-offtopic"
           "#wesnoth"
           "#themanaworld"))

 erc-server "irc.libera.chat"
 erc-port 6697
 erc-nick "alezost"
 erc-user-full-name user-full-name
 erc-server-reconnect-timeout 60
 erc-server-connect-function 'erc-open-tls-stream
 erc-prompt-for-password nil
 erc-hide-list '("JOIN" "QUIT")
 erc-mode-line-format "%t"
 erc-mode-line-away-status-format " (AWAY %a %H:%M)"
 erc-header-line-format "%n%a on %S [%m,%l] %o"
 erc-paranoid t
 erc-timestamp-format-left "\n[%d %B %Y, %A]\n"
 erc-insert-timestamp-function 'al/erc-insert-timestamp

 erc-ctcp-query-FINGER-hook  '(al/erc-ctcp-query-FINGER)
 erc-ctcp-query-ECHO-hook    '(al/erc-ctcp-query-ECHO)
 erc-ctcp-query-TIME-hook    '(al/erc-ctcp-query-TIME)
 erc-ctcp-query-VERSION-hook '(al/erc-ctcp-query-VERSION)

 erc-generate-log-file-name-function 'al/erc-log-file-name-network-channel
 erc-log-file-coding-system 'utf-8

 al/erc-log-excluded-regexps
 '("\\`#archlinux\\'" "\\`#emacs\\'" "\\`#freenode\\'" "\\`#znc\\'")
 al/erc-away-msg-list '("just away"))

(setq-default erc-enable-logging 'al/erc-log-all-but-some-buffers)

(defun al/erc-quit-part-reason (&rest _) "")
(setq
 erc-quit-reason 'al/erc-quit-part-reason
 erc-part-reason 'al/erc-quit-part-reason)

(when (al/znc-running-p)
  (setq erc-server "localhost"
        erc-port 32456))

;; Do not consider "'" a part of a symbol, so that `symbol-at-point'
;; (used by `elisp-slime-nav' functions) returns a proper symbol.
(al/modify-syntax erc-mode-syntax-table (?' "'   "))

(defvar al/tab-functions)
(push 'al/erc-next-button-maybe al/tab-functions)

(defun al/erc-channel-config ()
  "Define additional settings depending on a channel."
  (pcase (buffer-name (current-buffer))
    ((or "#scheme" "#guile")
     ;; Some hacks to make it possible to use guile process in erc
     ;; buffer.
     (setq-local
      geiser-impl--implementation 'guile
      geiser-eval--get-module-function (lambda (_module) :f)
      geiser-eval--geiser-procedure-function 'geiser-guile--geiser-procedure)
     (al/bind-local-keys-from-vars 'al/geiser-keys))
    ("#lisp"
     (al/bind-local-keys-from-vars 'al/sly-keys))
    ("#stumpwm"
     (setq-local sly-buffer-package :stumpwm)
     (al/bind-local-keys-from-vars 'al/sly-keys))))

(al/call-at-hook erc-mode-hook
  visual-line-mode
  abbrev-mode)
(add-hook 'erc-join-hook #'al/erc-channel-config)
(add-hook 'erc-after-connect #'al/erc-ghost-maybe)

(advice-add 'erc-notifications-notify :before #'al/play-erc-sound)

(al/eval-after-load erc-track
  (setq
   erc-track-showcount t
   erc-track-exclude-types
   '("JOIN" "NICK" "PART" "QUIT" "MODE"
     "305" "306"                ; away messages
     "324"                      ; channel modes
     "328"
     "329"                      ; channel was created on
     "332"                      ; welcome/topic messages
     "333"                      ; set topic
     "353" "477")))

(al/eval-after-load erc-match
  (setq erc-keywords '("theme" "color" "dvorak" "sql" "guix" "game")))

(al/eval-after-load erc-button
  (defconst al/erc-button-keys
    '("TAB"
      ("u" . erc-button-press-button)
      ("e" . erc-button-next)
      ("." . erc-button-previous)
      ("c"   (kill-new (car (get-text-property (point) 'erc-data))))
      ("w"   (wget (car (get-text-property (point) 'erc-data))))))
  (al/bind-keys-from-vars 'erc-button-keymap 'al/erc-button-keys))

(al/eval-after-load erc-list
  (al/bind-keys
   :map erc-list-menu-mode-map
   ("u"   . erc-list-join)
   ("RET" . erc-list-join))
  (define-key erc-list-menu-sort-button-map
    [header-line mouse-2] 'erc-list-menu-sort-by-column))

;;; erc.el ends here
