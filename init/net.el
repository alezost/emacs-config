;;; net.el --- Browsing, mail, chat, network utils; w3m, wget, …  -*- lexical-binding: t -*-

;; Copyright © 2014–2026 Alex Kost

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

(require 'al-places)
(require 'al-general)
(require 'al-key)

(declare-function al/gnus-dir-file "net.el")


;;; Global keys

(al/bind-key* "M-S" tui/web-search)

(al/bind-keys
 :prefix-map al/net-map
 :prefix-docstring "Map for net utils."
 :prefix "C-w"
 ("p" . al/ping)
 ("t" . al/traceroute)
 ("w" . wget)
 ("m" . al/url-wget-mp3))


;;; Browsing

(al/eval-after-load w3m
  (setq
   w3m-add-user-agent nil
   w3m-use-cookies nil
   w3m-confirm-leaving-secure-page nil
   w3m-use-title-buffer-name t  ; don't duplicate title in the mode-line
   w3m-show-graphic-icons-in-mode-line nil
   w3m-modeline-image-status-on "🌼"
   w3m-modeline-status-off ""
   w3m-modeline-separator "")

  (defconst al/w3m-keys
    '("c" "u" "k" "M-s" [left]
      ("<ctrl-m> a"  (emms-add-url (w3m-anchor)))
      ("<ctrl-m> p"  (emms-play-url (w3m-anchor)))
      ("i"         . w3m-toggle-inline-image)
      ("I"         . w3m-toggle-inline-images)
      ("b"         . w3m-bookmark-view)
      ("y"         . w3m-history)
      (","         . w3m-view-previous-page)
      ("p"         . w3m-view-next-page)
      ("h"         . al/w3m-previous-url)
      ("n"         . al/w3m-next-url)
      ("<backtab>" . w3m-previous-form)
      ("<tab>"     . w3m-next-form)
      ("R"         . w3m-redisplay-this-page)
      ("g"         . w3m-reload-this-page)
      ("j"         . w3m-goto-url)
      ("."         . w3m-previous-anchor)
      ("e"         . w3m-next-anchor)
      ("o"         . w3m-view-parent-page)
      ("O"           (w3m-view-parent-page 0))
      ("U"         . w3m-view-this-url-new-session)
      ("u 0"         (browse-url w3m-current-url))
      ("u u"         (browse-url (w3m-anchor)))
      ("u RET"       (browse-url (w3m-anchor)))
      ("c 0"       . w3m-print-current-url)
      ("c RET"     . w3m-print-this-url)
      ("s"         . al/w3m-wget)
      ("w"         . al/w3m-wget)
      ("C-w w"     . al/w3m-wget)
      ("C-w m"       (al/url-wget-mp3 (w3m-anchor)))
      ("C-c C-f"   . w3m-next-buffer)
      ("C-c C-b"   . w3m-previous-buffer))
    "Alist of auxiliary keys for `w3m-mode-map'.")
  (al/bind-keys-from-vars 'w3m-mode-map 'al/w3m-keys)

  (al/require al-w3m))

(al/eval-after-load w3m-save
  (setq
   w3m-save-buffer-html-only t
   w3m-default-save-directory (al/download-dir-file "html")))

(al/eval-after-load w3m-form
  (defconst al/w3m-form-keys
    '(("u" . w3m-form-input-select-set))
    "Alist of auxiliary keys for `w3m-form-input-select-keymap'.")
  (al/bind-keys-from-vars 'w3m-form-input-select-keymap
    '(al/lazy-moving-keys al/w3m-form-keys)))

(al/eval-after-load al-w3m
  (setq
   al/w3m-search-link-depth 20
   al/w3m-search-re "[^[:alnum:]]*\\<%s\\>")

  (al/w3m-bind-number-keys 'al/w3m-switch-to-buffer)
  (al/w3m-bind-number-keys 'al/w3m-kill-buffer "k"))

(al/eval-after-load browse-url
  (setq browse-url-browser-function 'tui/choose-browser)
  (al/require al-browse-url))

(al/eval-after-load al-browse-url
  (advice-add 'browse-url-default-browser
    :override #'al/browse-url-default))


;;; Mail, news, gnus

(setq
 mail-user-agent 'gnus-user-agent
 user-full-name "Alex Kost")

(defvar al/mail-user-name "alezost")

(al/eval-after-init
  ;; Append because `al/mail-user-name' can be changed later.
  :append t
  (al/file-accessors "gnus"
    (al/emacs-data-dir-file (concat "gnus-" al/mail-user-name)))
  (al/setq-no-warnings
   ;; Set `gnus-home-directory' before loading Gnus.  Otherwise,
   ;; `gnus-startup-file' will be set to "~/.newsrc" for some reason.
   gnus-home-directory al/gnus-dir
   user-mail-address (concat al/mail-user-name "@gmail.com")))

(al/bind-keys
 :prefix-map al/gnus-map
 :prefix-docstring "Map for Gnus."
 :prefix "M-g"
 ("M-g" . al/gnus-switch-win-config)
 ("g"   . al/gnus-switch-to-group-buffer)
 ("b"   . al/gnus-switch-buffer)
 ("m"   . gnus-msg-mail)
 ("n"   . gnus-msg-mail))

(al/eval-after-load nntp
  (setq nntp-connection-timeout 10))

(al/eval-after-load gnus
  (setq
   gnus-directory (al/gnus-dir-file "news")
   gnus-article-save-directory (al/gnus-dir-file "saved")
   gnus-update-message-archive-method t
   gnus-select-method '(nnml "")
   gnus-secondary-select-methods
   `((nnimap "gmail"
             (nnimap-user ,al/mail-user-name)
             (nnimap-address "imap.gmail.com")
             (nnimap-stream tls))
     ;; (nntp "gmane" (nntp-address "news.gmane.io"))
     )

   gnus-group-buffer "*Gnus Groups*"
   gnus-group-mode-line-format "Gnus:"
   gnus-summary-line-format "%U%R%z %(%&user-date; %B%-3L %[%f%]%) %s\n"
   gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"

   gnus-activate-level 3
   gnus-activate-foreign-newsgroups gnus-activate-level

   gnus-large-newsgroup 400)

  ;; Wrap text in gnus-article buffers by words.
  (add-hook 'gnus-article-mode-hook 'visual-line-mode)

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (al/add-hook-maybe 'dired-mode-hook 'turn-on-gnus-dired-mode)

  (al/require al-gnus))

(al/eval-after-load gnus-msg
  (setq gnus-gcc-mark-as-read t))

(al/eval-after-load mm-decode
  (setq
   mm-text-html-renderer 'gnus-w3m
   mm-discouraged-alternatives '("text/html" "text/richtext")))

(al/eval-after-load gnus-srvr
  (defconst al/gnus-server-keys
    '(("u"   . gnus-server-read-server)
      ("M-d" . gnus-server-edit-server))
    "Alist of auxiliary keys for `gnus-server-mode-map'.")
  (al/bind-keys-from-vars 'gnus-server-mode-map
    '(al/lazy-moving-keys al/gnus-server-keys)
    t)
  (al/bind-keys
   :map gnus-browse-mode-map
   ("." . gnus-browse-prev-group)
   ("e" . gnus-browse-next-group)
   ("u" . gnus-browse-select-group)
   ("U" . gnus-browse-unsubscribe-current-group)
   ("^" . gnus-browse-exit)))

;; `gnus-group-mode-map'/`gnus-summary-mode-map'/`gnus-article-mode-map'
;; are defined in "gnus.el" but are filled in
;; "gnus-group.el"/"gnus-sum.el"/"gnus-art.el".

(al/eval-after-load gnus-group
  (setq gnus-group-goto-unread nil)
  (defconst al/gnus-group-keys
    '(("." . gnus-group-prev-group)
      ("e" . gnus-group-next-group)
      (">" . al/gnus-group-prev-unread-group)
      ("E" . al/gnus-group-next-unread-group)
      ("u" . gnus-group-read-group)
      ("U" . gnus-group-unsubscribe-current-group)
      ("m" . gnus-group-mark-group)
      ("z" . gnus-group-unmark-group)
      ("Z" . gnus-group-unmark-all-groups)
      ("M-U" . gnus-group-unsubscribe-group)
      ("H i" . gnus-info-find-node)
      ("C-k" . gnus-group-kill-group)
      ("C-t" . gnus-group-kill-region)
      ("H-u" . gnus-undo)
      ("<backtab>" . gnus-topic-unindent)
      ("M-." . gnus-topic-goto-previous-topic)
      ("M-e" . gnus-topic-goto-next-topic))
    "Alist of auxiliary keys for `gnus-group-mode-map'.")
  (al/bind-keys-from-vars 'gnus-group-mode-map 'al/gnus-group-keys)

  (add-hook 'gnus-group-mode-hook 'hl-line-mode))

(al/eval-after-load gnus-sum
  (setq
   gnus-summary-mode-line-format "Gnus: %p %Z"
   gnus-user-date-format-alist
   '(((gnus-seconds-today)           . "Today  %H:%M")
     ((+ 86400 (gnus-seconds-today)) . "Yest.  %H:%M")
     ((* 86400 365)                  . "%d %b %H:%M")
     (t                              . "%Y-%m-%d  "))
   gnus-subthread-sort-functions '(gnus-thread-sort-by-number
                                   gnus-thread-sort-by-date)
   gnus-sum-thread-tree-root            "●─► "
   gnus-sum-thread-tree-false-root      "○─► "
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-leaf-with-other "├─► "
   gnus-sum-thread-tree-single-leaf     "└─► "
   gnus-sum-thread-tree-indent          " "
   gnus-sum-thread-tree-single-indent   "■ "
   gnus-summary-newsgroup-prefix        "⇒ "
   gnus-summary-to-prefix               "→ "

   gnus-score-over-mark ?↑
   gnus-score-below-mark ?↓
   gnus-unseen-mark ?n
   gnus-read-mark ?✓
   gnus-killed-mark ?✗)

  (defconst al/gnus-summary-keys
    '("x" ; disable `gnus-summary-limit-to-unread'
      "M-r"
      ("."     . gnus-summary-prev-article)
      ("e"     . gnus-summary-next-article)
      (">"     . gnus-summary-prev-unread-article)
      ("E"     . gnus-summary-next-unread-article)
      ("D"     . gnus-summary-delete-article)
      ("n"     . gnus-summary-reply)
      ("m"     . gnus-summary-mark-as-read-forward)
      ("r"     . gnus-summary-mark-as-read-forward)
      ("z"     . gnus-summary-clear-mark-forward)
      ("u"     . gnus-summary-scroll-up)
      ("C-t"   . gnus-summary-mark-region-as-read)
      ("h"     . gnus-summary-toggle-header)
      ("b"     . al/gnus-summary-toggle-display-buttonized)
      ("v"     . gnus-article-view-part)
      ("V"     . gnus-mime-view-all-parts)
      ("s"     . gnus-article-save-part)
      ("i"     . gnus-article-show-images)
      ("U"     . al/gnus-summary-browse-link-url)
      ("a"     . al/gnus-summary-emms-add-url)
      ("p"     . al/gnus-summary-emms-play-url)
      ("<ctrl-m> a" . al/gnus-summary-emms-add-url)
      ("<ctrl-m> p" . al/gnus-summary-emms-play-url)
      ("w"       (wget (al/gnus-summary-find-mm-url))))
    "Alist of auxiliary keys for `gnus-summary-mode'.")
  (al/bind-keys-from-vars 'gnus-summary-mode-map 'al/gnus-summary-keys)

  (al/bind-keys
    :map gnus-summary-mode-map
    :prefix-map al/gnus-summary-search-map
    :prefix-docstring "Search map for `gnus-summary-mode'."
    :prefix "M-s"
    ("M-s" . gnus-summary-search-article-forward)
    ("M-r" . gnus-summary-search-article-backward))

  (al/add-hook-maybe 'gnus-summary-mode-hook
    '(hl-line-mode al/hbar-cursor-type)))

(al/eval-after-load gnus-draft
  (defconst al/gnus-draft-keys
    '(("M-d" . gnus-draft-edit-message)
      "e")
    "Alist of auxiliary keys for `gnus-draft-mode-map'.")
  (al/bind-keys-from-vars 'gnus-draft-mode-map 'al/gnus-draft-keys))

(al/eval-after-load gnus-art
  (setq
   gnus-treat-display-smileys nil
   gnus-article-truncate-lines nil
   gnus-article-mode-line-format "Gnus: %m"
   gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^User-Agent:"
   gnus-unbuttonized-mime-types '("text/plain")
   gnus-blocked-images "githubusercontent"
   gnus-prompt-before-saving t
   gnus-default-article-saver 'gnus-summary-save-in-mail)

  (defconst al/gnus-article-keys
    '("C-d")
    "Alist of auxiliary keys for `gnus-article-mode-map'.")
  (defconst al/gnus-url-button-keys
    '(("c" . gnus-article-copy-string))
    "Alist of auxiliary keys for `gnus-url-button-map'.")
  (defconst al/gnus-mime-button-keys
    '(("u" . gnus-mime-action-on-part)
      ("s" . gnus-mime-save-part)
      ("v" . gnus-mime-view-part-internally)
      ("V" . gnus-mime-view-part))
    "Alist of auxiliary keys for `gnus-mime-button-map'.")
  (al/bind-keys-from-vars 'gnus-article-mode-map
    '(al/button-keys al/gnus-article-keys))
  (al/bind-keys-from-vars 'gnus-url-button-map
    '(al/button-keys al/gnus-url-button-keys))
  (al/bind-keys-from-vars 'gnus-mime-button-map
    '(al/button-keys al/gnus-mime-button-keys))
  )

(al/eval-after-load gnus-topic
  (setq
   gnus-topic-display-empty-topics nil
   gnus-topic-line-format "%i%(%{%n%}%) – %A %v\n")
  (al/bind-keys-from-vars 'gnus-topic-mode-map
    'al/free-important-keys t))

(al/eval-after-load gnus-dired
  (al/bind-keys
   :map gnus-dired-mode-map
   ("C-c a" . gnus-dired-attach)))

(al/eval-after-load sendmail
  (setq
   send-mail-function 'smtpmail-send-it))

(al/eval-after-load message
  (setq
   message-directory (al/gnus-dir-file "mail")
   message-signature "Alex"
   message-send-mail-function 'smtpmail-send-it
   message-citation-line-function 'message-insert-formatted-citation-line
   message-citation-line-format "%N (%Y-%m-%d %H:%M %z) wrote:\n")
  (al/bind-keys-from-vars 'message-mode-map)
  (al/modify-syntax message-mode-syntax-table
    (?' "'   ")
    (?\" "\"   ")))

(al/eval-after-load mml
  (defconst al/mml-keys
    '(("C-c a" . mml-attach-file)
      ("C-c f" . mml-attach-file)
      ("C-c b" . mml-attach-buffer)
      ("C-c P" . mml-preview))
    "Alist of auxiliary keys for `mml-mode-map'.")
  (al/bind-keys-from-vars 'mml-mode-map 'al/mml-keys))

(al/eval-after-load smtpmail
  (setq
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587))

(al/eval-after-load shr
  (al/bind-keys
   :map shr-map
   ("u" . shr-browse-url)
   ("c" . shr-copy-url)))

(al/eval-after-load al-gnus
  (setq al/atom2rss-file (al/emacs-data-dir-file "atom2rss.xsl"))
  (advice-add 'mm-url-insert
    :after #'al/convert-atom-to-rss)
  (advice-add 'gnus-agent-make-mode-line-string
    :around #'al/gnus-agent-mode-line-string))


;;; ERC

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
   truncate)
 ;; Set `erc-autojoin-channels-alist' in the top level so that it can be
 ;; changed before loading ERC.
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
    "#conkeror"
    "#stumpwm"
    "#openmw"
    "#org-mode"))
 erc-log-channels-directory (al/emacs-data-dir-file "erc-log"))

(declare-function erc-part-from-channel "erc")

(al/bind-keys*
 :prefix-map al/erc-map
 :prefix-docstring "Map for ERC."
 :prefix "M-c"
 ("M-c" . al/erc-track-switch-buffer)
 ("M-n" . al/erc-cycle)
 ("l"   . al/erc-channel-list)
 ("b"   . al/erc-switch-buffer)
 ("M-s" . al/erc-switch-to-server-buffer)
 ;; Interactive erc - compute everything without prompting:
 ("c"     (erc))
 ("R"   . al/erc-server-buffer-rename)
 ("d"   . al/erc-quit-server)
 ("j"   . al/erc-join-channel)
 ("a"   . al/erc-away)
 ("u"   . al/erc-number-of-users)
 ("m"   . erc-track-mode)
 ("n"   . erc-notifications-mode)
 ("p"     (erc-part-from-channel ""))
 ("e"     (al/display-buffer "#emacs"))
 ("x"     (al/display-buffer "#guix"))
 ("s"     (al/display-buffer "#stumpwm"))
 ("M-z"   (al/display-buffer "*status")))

(al/eval-after-load erc
  (setq
   erc-server "irc.libera.chat"
   erc-port 6697
   erc-nick "alezost"
   erc-user-full-name user-full-name
   erc-server-reconnect-timeout 60
   erc-server-connect-function 'erc-open-tls-stream
   ;; erc-join-buffer 'bury
   erc-prompt-for-password nil
   erc-hide-list '("JOIN" "QUIT")
   erc-mode-line-format "%t"
   erc-mode-line-away-status-format " (AWAY %a %H:%M)"
   erc-header-line-format "%n%a on %S [%m,%l] %o"
   erc-paranoid t)

  (defun al/erc-quit-part-reason (&rest _)
    "I live in Emacs <https://www.gnu.org/software/emacs/>")
  (setq
   erc-quit-reason 'al/erc-quit-part-reason
   erc-part-reason 'al/erc-quit-part-reason)

  (defconst al/erc-keys
    '("TAB"
      ("M-." . erc-previous-command)
      ("M-e" . erc-next-command)
      ("C-a" . erc-bol)
      ("C-c C-d" . al/erc-part-or-quit)
      ("C-l" . al/erc-view-log-file)
      ("<s-kanji>" . al/recenter-end-of-buffer-top)
      ("C-H-3" . al/recenter-end-of-buffer-top)))

  ;; This auxiliary function is needed because some modules (`erc-ring')
  ;; add their key bindings to `erc-mode-map'.
  (defun al/erc-bind-keys ()
    (al/bind-keys-from-vars 'erc-mode-map 'al/erc-keys))
  (al/add-hook-maybe 'erc-ring-mode-hook 'al/erc-bind-keys)

  (al/add-hook-maybe 'erc-mode-hook
    '(visual-line-mode abbrev-mode))

  ;; Do not consider "'" a part of a symbol, so that `symbol-at-point'
  ;; (used by `elisp-slime-nav' functions) returns a proper symbol.
  (al/modify-syntax erc-mode-syntax-table (?' "'   "))

  (defun al/erc-channel-config ()
    "Define additional settings depending on a channel."
    (let ((buf (buffer-name (current-buffer))))
      (cond
       ((or (string-match "#scheme" buf)
            (string-match "#guile" buf))
        ;; Some hacks to make it possible to use guile process in erc
        ;; buffer.
        (setq-local geiser-impl--implementation 'guile)
        (setq-local geiser-eval--get-module-function
                    (lambda (_module) :f))
        (setq-local geiser-eval--geiser-procedure-function
                    'geiser-guile--geiser-procedure)
        (al/bind-local-keys-from-vars 'al/geiser-keys))
       ((string-match "#lisp" buf)
        (al/bind-local-keys-from-vars 'al/slime-keys))
       ((string-match "#stumpwm" buf)
        (setq-local slime-buffer-package :stumpwm)
        (al/bind-local-keys-from-vars 'al/sly-keys)))))
  (al/add-hook-maybe 'erc-join-hook 'al/erc-channel-config)

  (al/require al-erc))


(al/eval-after-load al-erc
  (when (al/znc-running-p)
    (setq erc-server "localhost"
          erc-port 32456))
  (setq-default erc-enable-logging 'al/erc-log-all-but-some-buffers)
  (setq
   erc-insert-timestamp-function 'al/erc-insert-timestamp
   erc-generate-log-file-name-function
   'al/erc-log-file-name-network-channel)
  (setq
   erc-ctcp-query-FINGER-hook  '(al/erc-ctcp-query-FINGER)
   erc-ctcp-query-ECHO-hook    '(al/erc-ctcp-query-ECHO)
   erc-ctcp-query-TIME-hook    '(al/erc-ctcp-query-TIME)
   erc-ctcp-query-VERSION-hook '(al/erc-ctcp-query-VERSION))
  (setq
   al/erc-log-excluded-regexps
   '("\\`#archlinux\\'" "\\`#emacs\\'" "\\`#freenode\\'" "\\`#znc\\'")
   al/erc-away-msg-list
   '("just away" "watching athletics" "watching darts"
     "eating" "i'm not ready to chat" "time to sleep")
   al/erc-channel-list
   '("#emacs" "#archlinux" "#archlinux-classroom" "#trivialand" "##latin"
     "#lisp" "#lispgames" "#git" "#github" "#netfilter" "#wesnoth"
     "#themanaworld" "##french" "##english" "##programming"))

  (defvar al/tab-functions)
  (push 'al/erc-next-button-maybe al/tab-functions)

  (al/add-hook-maybe 'erc-after-connect 'al/erc-ghost-maybe)
  (advice-add 'erc-notifications-notify :before #'al/play-erc-sound))

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

(al/eval-after-load erc-stamp
  (setq
   erc-timestamp-format-left "\n[%d %B %Y, %A]\n"))

(al/eval-after-load erc-log
  (setq erc-log-file-coding-system 'utf-8))

(al/eval-after-load erc-button
  (defconst al/erc-button-keys
    '("TAB"
      ("u" . erc-button-press-button)
      ("e" . al/next-link)
      ("." . al/previous-link)
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

(al/autoload "erc-view-log" erc-view-log-mode)
(al/with-check
  :var 'erc-log-channels-directory
  (push (cons (concat "\\`"
                      (regexp-quote (expand-file-name
                                     erc-log-channels-directory)))
              'erc-view-log-mode)
        auto-mode-alist))

(al/eval-after-load erc-view-log
  (setq
   erc-view-log-timestamp-regexp
   (rx "[" (one-or-more (or digit ":")) "]")
   erc-view-log-timestamp-position 'left
   ))


;;; Debbugs

(al/bind-keys
 :prefix-map al/debbugs-map
 :prefix-docstring "Map for debbugs."
 :prefix "M-B"
 ("M-B" . debbugs-gnu)
 ("n"   . debbugs-gnu-bugs)
 ("b"     (al/display-buffer "*Guix-Patches Bugs*"))
 ("s"   . debbugs-gnu-search))

(al/eval-after-load debbugs-gnu
  (setq debbugs-gnu-default-packages '("guix-patches"))

  (defconst al/debbugs-gnu-keys
    '(("u" . debbugs-gnu-select-report))
    "Alist of auxiliary keys for `debbugs-gnu-mode-map'.")
  (al/bind-keys-from-vars 'debbugs-gnu-mode-map 'al/debbugs-gnu-keys))


;;; Misc settings and packages

(setq url-debug t)
(al/eval-after-load url
  (setq url-configuration-directory (al/emacs-data-dir-file "url")))

(al/eval-after-load wget
  (setq
   wget-debug-buffer "*wget-log*"
   wget-download-directory-filter 'wget-download-dir-filter-regexp
   wget-download-log-file (al/emacs-data-dir-file "emacs-wget.log")))

(al/eval-after-load net-utils
  (setq ping-program-options '("-c" "3")))

(al/eval-after-load al-net
  (setq
   al/net-hosts '("zeus" "leviafan" "hyperion" "192.168.1.1"
                  "google.com" "ya.ru")
   al/router-log-directory "~/docs/net/router-log/"))

(al/bind-keys
 :prefix-map al/debpaste-map
 :prefix-docstring "Map for debpaste."
 :prefix "C-H-p"
 ("s" . debpaste-paste-region)
 ("r" . debpaste-display-paste)
 ("S" . debpaste-display-posted-info-in-buffer)
 ("R" . debpaste-display-received-info-in-buffer)
 ("d" . debpaste-delete-paste)
 ("q" . debpaste-quit-buffers)
 ("K" . debpaste-kill-all-buffers))

(al/eval-after-load debpaste
  (setq
   debpaste-user-name "alezost"
   debpaste-expire-time (* 3 24 60 60))
  (add-to-list 'debpaste-domains "debpaste" t))

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

;;; net.el ends here
