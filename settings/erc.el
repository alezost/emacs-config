;;; erc.el --- Settings for `erc' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'erc)
(require 'erc-join)
(require 'erc-log)
(require 'al-key)
(require 'al-erc)

(al/bind-keys
  :map erc-mode-map
  ("M-↑" 'erc-previous-command)
  ("M-↓" 'erc-next-command)
  ("C-⇤" 'erc-bol)
  ("C-l" 'al/erc-view-log-file)
  ("C-c C-d" 'al/erc-part-or-quit))

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


;;; Configuring SLY and Geiser for ERC channel buffers

(defvar al/erc-geiser-mode-map (make-sparse-keymap))

(define-minor-mode al/erc-geiser-mode
  "Make `al/geiser-map' keys available (after `geiser' is loaded)."
  :lighter ""
  (if al/erc-geiser-mode
      (setq-local
       geiser-impl--implementation 'guile
       geiser-eval--get-module-function (lambda (_module) :f)
       geiser-eval--geiser-procedure-function
       'geiser-guile--geiser-procedure)
    (kill-local-variable 'geiser-impl--implementation)
    (kill-local-variable 'geiser-eval--get-module-function)
    (kill-local-variable 'geiser-eval--geiser-procedure-function)))

(defvar al/geiser-map)
(al/eval-after-load geiser-mode
  (al/bind-keys
    :map al/erc-geiser-mode-map
    :parent al/geiser-map))

(defvar al/erc-sly-mode-map (make-sparse-keymap))

(define-minor-mode al/erc-sly-mode
  "Make `al/sly-map' keys available (after `sly' is loaded)."
  :lighter "")

(defvar al/sly-map)
(al/eval-after-load sly
  (al/bind-keys
    :map al/erc-sly-mode-map
    :parent al/sly-map))

(defun al/erc-channel-config ()
  "Define additional settings depending on a channel."
  (pcase (buffer-name (current-buffer))
    ((or "#scheme" "#guile")
     (al/erc-geiser-mode))
    ("#lisp"
     (al/erc-sly-mode))
    ("#stumpwm"
     (setq-local sly-buffer-package :stumpwm)
     (al/erc-sly-mode))))

(add-hook 'erc-join-hook #'al/erc-channel-config)



(al/call-at-hook erc-mode-hook
  visual-line-mode
  abbrev-mode)
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
  (al/bind-keys
    :map erc-button-keymap
    ("→" 'erc-button-press-button)
    ("↓" 'erc-button-next)
    ("↑" 'erc-button-previous)
    ("c" (kill-new (car (get-text-property (point) 'erc-data))))
    ("w" (wget (car (get-text-property (point) 'erc-data))))))

(al/eval-after-load erc-list
  (al/bind-keys
    :map erc-list-menu-mode-map
    ("→" 'erc-list-join)
    ("RET" 'erc-list-join))

  (al/bind-keys
    :map erc-list-menu-sort-button-map
    ([header-line mouse-2] 'erc-list-menu-sort-by-column)))

;;; erc.el ends here
