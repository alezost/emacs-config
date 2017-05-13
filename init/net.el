;;; net.el --- Browsing, mail, chat, network utils; w3m, wget, …

;; Copyright © 2014–2017 Alex Kost

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


;;; Global keys

(al/bind-keys
 :prefix-map al/net-map
 :prefix-docstring "Map for net utils."
 :prefix "C-w"
 ("p" . al/ping)
 ("t" . al/traceroute)
 ("w" . wget)
 ("m" . al/url-wget-mp3))

(al/bind-keys*
 :prefix-map al/web-search-map
 :prefix-docstring "Map for web-search commands and browsing URLs."
 :prefix "M-S"
 ("M-S" . web-search)
 ("d"   . web-search-duckduckgo)
 ("y"   . web-search-yandex)
 ("g"   . web-search-github)
 ("G"   . web-search-google)
 ("w e" . web-search-wikipedia-en)
 ("w r" . web-search-wikipedia-ru)
 ("W"   . web-search-wiktionary-en)
 ("m"   . web-search-multitran)
 ("a"   . web-search-archwiki)
 ("A"   . web-search-arch-package)
 ("e"   . web-search-emacswiki)
 ("i"   . web-search-ipduh)
 ("I"   . web-search-ip-address)
 ("b"   . web-search-debbugs)
 ("`"   . web-search-ej)
 ("t"     (w3m-browse-url "http://m.tv.yandex.ru/4"))
 ("l"   . al/browse-irc-log)
 ("L"     (al/browse-irc-log
           "guix"
           (format-time-string
            "%Y-%m-%d"
            (time-subtract (current-time)
                           (seconds-to-time (* 24 60 60)))))))


;;; Browsing

(with-eval-after-load 'w3m
  (setq
   w3m-confirm-leaving-secure-page nil
   w3m-use-title-buffer-name t  ; don't duplicate title in the mode-line
   w3m-show-graphic-icons-in-mode-line nil
   w3m-modeline-image-status-on "🌼"
   w3m-modeline-status-off ""
   w3m-modeline-separator "")

  (defconst al/w3m-keys
    '("c" "u" "k" "M-s"
      ("C-ь a"       (emms-add-url (w3m-anchor)))
      ("C-ь p"       (emms-play-url (w3m-anchor)))
      ("i"         . w3m-toggle-inline-images)
      ("I"         . w3m-toggle-inline-image)
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
      ("u v"         (browse-url-default-browser
                      (echo-msk-program-video-url (w3m-anchor))))
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

  (when (require 'al-w3m nil t)
    (al/w3m-bind-number-keys 'al/w3m-switch-to-buffer)
    (al/w3m-bind-number-keys 'al/w3m-kill-buffer "k")))

(with-eval-after-load 'w3m-form
  (defconst al/w3m-form-keys
    '(("u" . w3m-form-input-select-set))
    "Alist of auxiliary keys for `w3m-form-input-select-keymap'.")
  (al/bind-keys-from-vars 'w3m-form-input-select-keymap
    '(al/lazy-moving-keys al/w3m-form-keys)))

(with-eval-after-load 'al-w3m
  (setq
   al/w3m-search-link-depth 20
   al/w3m-search-re "[^[:alnum:]]*\\<%s\\>"))

(with-eval-after-load 'browse-url
  (when (require 'al-browse-url nil t)
    (setq browse-url-browser-function 'al/choose-browser)
    (advice-add 'browse-url-default-browser
      :override 'al/browse-url-conkeror)))

(with-eval-after-load 'al-browse-url
  (setcar (cl-find-if (lambda (spec)
                        (string= "conkeror" (cadr spec)))
                      al/browser-choices)
          '(?c ?u ?\C-m)))


;;; Mail, news, gnus

(setq
 user-full-name "Alex Kost"
 user-mail-address (concat "alezost" '(?@ ?g) "mail" '(?.) "com"))

(setq
 gnus-home-directory al/gnus-dir
 gnus-directory      al/gnus-news-dir
 message-directory   al/gnus-mail-dir
 ;; gnus-message-archive-group "sent"
 gnus-update-message-archive-method t)

(setq mail-user-agent 'gnus-user-agent)

(al/bind-keys
 :prefix-map al/gnus-map
 :prefix-docstring "Map for Gnus."
 :prefix "M-g"
 ("M-g" . al/gnus-switch-win-config)
 ("g"   . al/gnus-switch-to-group-buffer)
 ("b"   . al/gnus-switch-buffer)
 ("m"   . gnus-msg-mail)
 ("n"   . gnus-msg-mail))

(with-eval-after-load 'nntp
  (setq nntp-connection-timeout 10))

(with-eval-after-load 'gnus
  (require 'al-gnus nil t)
  (setq
   gnus-select-method '(nnml "")
   gnus-secondary-select-methods
   '((nnimap "gmail"
             (nnimap-address "imap.gmail.com")
             (nnimap-server-port 993)
             (nnimap-stream ssl))
     (nntp "gmane" (nntp-address "news.gmane.org"))))

  (setq
   gnus-group-buffer "*Gnus Groups*"
   gnus-group-mode-line-format "Gnus:"
   gnus-article-mode-line-format "Gnus: %m"
   gnus-summary-mode-line-format "Gnus: %p %Z"
   gnus-summary-line-format "%U%R%z %(%&user-date; %B%-3L %[%f%]%) %s\n"
   gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
   gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^User-Agent:"
   gnus-user-date-format-alist
   '(((gnus-seconds-today)           . "Today  %H:%M")
     ((+ 86400 (gnus-seconds-today)) . "Yest.  %H:%M")
     ((* 86400 365)                  . "%d %b %H:%M")
     (t                              . "%Y-%m-%d  "))
   gnus-subthread-sort-functions '(gnus-thread-sort-by-number
                                   gnus-thread-sort-by-date))

  (setq
   gnus-activate-level 3
   gnus-activate-foreign-newsgroups gnus-activate-level)

  (setq
   gnus-large-newsgroup 400
   gnus-treat-display-smileys nil
   mm-text-html-renderer 'gnus-w3m
   mm-inline-text-html-with-images t
   gnus-gcc-mark-as-read t)

  ;; Wrap text in gnus-article buffers by words.
  (add-hook 'gnus-article-mode-hook 'visual-line-mode)
  (setq gnus-article-truncate-lines nil)

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (al/add-hook-maybe 'dired-mode-hook 'turn-on-gnus-dired-mode))

(with-eval-after-load 'gnus-srvr
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

(with-eval-after-load 'gnus-group
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

(with-eval-after-load 'gnus-sum
  (defvar al/ej-url-re "www\\.ej\\.ru.+id=\\([0-9]+\\)"
    "Regexp matching 'ej.ru' arcticles.")

  (setq
   gnus-sum-thread-tree-root            "●─► "
   gnus-sum-thread-tree-false-root      "○─► "
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-leaf-with-other "├─► "
   gnus-sum-thread-tree-single-leaf     "└─► "
   gnus-sum-thread-tree-indent          " "
   gnus-sum-thread-tree-single-indent   "■ "
   gnus-summary-newsgroup-prefix        "⇒ "
   gnus-summary-to-prefix               "→ ")

  (setq
   gnus-score-over-mark ?↑
   gnus-score-below-mark ?↓
   gnus-unseen-mark ?n
   gnus-read-mark ?✓
   gnus-killed-mark ?✗)

  (defconst al/gnus-summary-keys
    '("x" ; disable `gnus-summary-limit-to-unread'
      ("."     . gnus-summary-prev-article)
      ("e"     . gnus-summary-next-article)
      (">"     . gnus-summary-prev-unread-article)
      ("E"     . gnus-summary-next-unread-article)
      ("n"     . gnus-summary-reply)
      ("m"     . gnus-summary-mark-as-read-forward)
      ("r"     . gnus-summary-mark-as-read-forward)
      ("z"     . gnus-summary-clear-mark-forward)
      ("u"     . gnus-summary-scroll-up)
      ("C-t"   . gnus-summary-mark-region-as-read)
      ("b"     . al/gnus-summary-toggle-display-buttonized)
      ("v"     . gnus-article-view-part)
      ("V"     . gnus-mime-view-all-parts)
      ("s"     . gnus-article-save-part)
      ("i"     . gnus-article-show-images)
      ("U"     . al/gnus-summary-browse-link-url)
      ("a"     . al/gnus-summary-emms-add-url)
      ("p"     . al/gnus-summary-emms-play-url)
      ("C-ь a" . al/gnus-summary-emms-add-url)
      ("C-ь p" . al/gnus-summary-emms-play-url)
      ("w"       (wget (al/gnus-summary-find-mm-url)))
      ("`"       (web-search-ej (al/gnus-summary-find-url-by-re
                                 al/ej-url-re 1))))
    "Alist of auxiliary keys for `gnus-summary-mode'.")
  (al/bind-keys-from-vars 'gnus-summary-mode-map 'al/gnus-summary-keys)

  (al/add-hook-maybe 'gnus-summary-mode-hook
    '(hl-line-mode al/hbar-cursor-type)))

(with-eval-after-load 'gnus-draft
  (defconst al/gnus-draft-keys
    '(("M-d" . gnus-draft-edit-message)
      "e")
    "Alist of auxiliary keys for `gnus-draft-mode-map'.")
  (al/bind-keys-from-vars 'gnus-draft-mode-map 'al/gnus-draft-keys))

(with-eval-after-load 'gnus-art
  (setq
   gnus-unbuttonized-mime-types '("text/plain")
   gnus-blocked-images "githubusercontent"
   gnus-prompt-before-saving t
   gnus-default-article-saver 'gnus-summary-save-in-mail
   ;; `gnus-article-save-directory' is placed in "gnus.el" actually, but
   ;; I don't care.
   gnus-article-save-directory al/gnus-saved-dir)

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
    '(al/widget-button-keys al/gnus-article-keys))
  (al/bind-keys-from-vars 'gnus-url-button-map
    '(al/widget-button-keys al/gnus-url-button-keys))
  (al/bind-keys-from-vars 'gnus-mime-button-map
    '(al/widget-button-keys al/gnus-mime-button-keys))

  (add-hook 'gnus-article-mode-hook
            (lambda () (setq-local widget-button-face nil))))

(with-eval-after-load 'gnus-topic
  (setq
   gnus-topic-display-empty-topics nil
   gnus-topic-line-format "%i%(%{%n%}%) – %A %v\n")
  (al/bind-keys-from-vars 'gnus-topic-mode-map
    'al/free-important-keys t))

(with-eval-after-load 'gnus-dired
  (al/bind-keys
   :map gnus-dired-mode-map
   ("C-c a" . gnus-dired-attach)))

(with-eval-after-load 'sendmail
  (setq
   send-mail-function 'smtpmail-send-it))

(with-eval-after-load 'message
  (setq
   message-signature "Alex"
   message-send-mail-function 'smtpmail-send-it
   message-citation-line-function 'message-insert-formatted-citation-line
   message-citation-line-format "%N (%Y-%m-%d %H:%M %z) wrote:\n")

  (al/modify-syntax message-mode-syntax-table
    (?' "'   ")
    (?\" "\"   ")))

(with-eval-after-load 'mml
  (defconst al/mml-keys
    '(("C-c a" . mml-attach-file)
      ("C-c f" . mml-attach-file)
      ("C-c b" . mml-attach-buffer)
      ("C-c P" . mml-preview))
    "Alist of auxiliary keys for `mml-mode-map'.")
  (al/bind-keys-from-vars 'mml-mode-map 'al/mml-keys))

(with-eval-after-load 'smtpmail
  (setq
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587))

(with-eval-after-load 'shr
  (al/bind-keys
   :map shr-map
   ("u" . shr-browse-url)
   ("c" . shr-copy-url)))

(with-eval-after-load 'al-gnus
  (setq al/atom2rss-file (al/emacs-data-dir-file "atom2rss.xsl"))
  (advice-add 'mm-url-insert
    :after #'al/convert-atom-to-rss)
  (advice-add 'gnus-agent-make-mode-line-string
    :around #'al/gnus-agent-mode-line-string))

(al/autoload "mu4e" mu4e)
(with-eval-after-load 'mu4e
  (setq
   mu4e-maildir (expand-file-name "~/mail")
   mu4e~main-buffer-name "*mu4e-main*"))


;;; ERC

(setq erc-modules
      '(truncate keep-place log pcomplete netsplit button match
                 notifications track completion readonly networks ring autojoin
                 noncommands irccontrols move-to-prompt stamp menu list))
(setq erc-log-channels-directory (al/emacs-data-dir-file "erc-log"))

(al/bind-keys*
 :prefix-map al/erc-map
 :prefix-docstring "Map for ERC."
 :prefix "M-c"
 ("M-c" . al/erc-track-switch-buffer)
 ("M-n" . al/erc-cycle)
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
 ("e"     (switch-to-buffer "#emacs"))
 ("x"     (switch-to-buffer "#guix"))
 ("s"     (switch-to-buffer "#stumpwm"))
 ("M-z"   (switch-to-buffer "*status")))

(with-eval-after-load 'erc
  (setq
   erc-server "chat.freenode.net"
   erc-port 7000
   erc-nick "alezost"
   erc-user-full-name user-full-name
   erc-server-reconnect-timeout 60
   erc-server-connect-function 'erc-open-tls-stream
   ;; erc-join-buffer 'bury
   erc-track-showcount t
   erc-prompt-for-password nil
   erc-hide-list '("JOIN" "QUIT")
   erc-track-exclude-types
   '("JOIN" "NICK" "PART" "QUIT" "MODE"
     "305" "306"                        ; away messages
     "324"                              ; channel modes
     "328"
     "329"                              ; channel was created on
     "332"                              ; welcome/topic messages
     "333"                              ; set topic
     "353" "477")
   erc-mode-line-format "%t"
   erc-mode-line-away-status-format " (AWAY %a %H:%M)"
   erc-header-line-format "%n%a on %S [%m,%l] %o"
   erc-timestamp-format-left "\n[%d %B %Y, %A]\n"
   erc-timestamp-intangible nil
   erc-keywords '("theme" "color" "dvorak" "sql" "guix" "game")
   erc-log-file-coding-system 'utf-8
   erc-paranoid t
   erc-autojoin-channels-alist
   '(("freenode.net" "#emacs" "#erc" "#gnus" "#scheme" "#guile" "#guix"
      "#geiser" "#conkeror" "#stumpwm" "#org-mode"
      "#themanaworld" "#lgn")))

  (defun al/erc-quit-part-reason (&rest _)
    (concat "I live in GuixSD <http://www.gnu.org/s/guix>"
            " and Emacs <http://www.gnu.org/s/emacs>"))
  (setq
   erc-quit-reason 'al/erc-quit-part-reason
   erc-part-reason 'al/erc-quit-part-reason)

  (defconst al/erc-keys
    '(("<tab>" . pcomplete)
      ("M-." . erc-previous-command)
      ("M-e" . erc-next-command)
      ("C-a" . erc-bol)
      ("C-l" . al/erc-view-log-file)
      ("<s-kanji>" . al/recenter-end-of-buffer-top)
      ("C-H-3" . al/recenter-end-of-buffer-top))
    "Alist of auxiliary keys for erc mode.")
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
                    (lambda (module) :f))
        (setq-local geiser-eval--geiser-procedure-function
                    'geiser-guile--geiser-procedure)
        (al/bind-local-keys-from-vars 'al/geiser-keys))
       ((string-match "#lisp" buf)
        (al/bind-local-keys-from-vars 'al/slime-keys))
       ((string-match "#stumpwm" buf)
        (setq-local slime-buffer-package :stumpwm)
        (al/bind-local-keys-from-vars 'al/slime-keys)))))
  (al/add-hook-maybe 'erc-join-hook 'al/erc-channel-config)

  (when (require 'al-erc nil t)
    (when (al/znc-running-p)
      (setq erc-server "localhost"
            erc-port 32456))
    (setq-default erc-enable-logging 'al/erc-log-all-but-some-buffers)
    (setq
     erc-insert-timestamp-function 'al/erc-insert-timestamp
     erc-view-log-timestamp-position 'left
     erc-generate-log-file-name-function
     'al/erc-log-file-name-network-channel)
    (setq
     erc-ctcp-query-FINGER-hook  '(al/erc-ctcp-query-FINGER)
     erc-ctcp-query-ECHO-hook    '(al/erc-ctcp-query-ECHO)
     erc-ctcp-query-TIME-hook    '(al/erc-ctcp-query-TIME)
     erc-ctcp-query-VERSION-hook '(al/erc-ctcp-query-VERSION))
    (al/add-hook-maybe 'erc-after-connect 'al/erc-ghost-maybe))

  (when (require 'sauron nil t)
    (setq sauron-watch-patterns
          (append sauron-watch-patterns
                  '("theme" "color" "debpaste" "guix\\.el"
                    "game" "ducpel" "sokoban")))
    (add-to-list 'sauron-modules 'sauron-erc))
  ;; ERC is loaded twice somehow (why??); so clear erc assoc of
  ;; `after-load-alist' to prevent the second loading of these settings.
  (setq after-load-alist
        (assq-delete-all 'erc after-load-alist)))

(with-eval-after-load 'erc-desktop-notifications
  (setq erc-notifications-icon "erc")
  (defun al/play-erc-sound (&rest _)
    (require 'al-sound)
    (al/play-sound (al/sound-dir-file "chimes.wav")))
  (advice-add 'erc-notifications-notify :before #'al/play-erc-sound))

(with-eval-after-load 'erc-button
  (al/bind-keys
   :map erc-button-keymap
   ("u" . erc-button-press-button)
   ("e" . al/next-link)
   ("." . al/previous-link)
   ("c"   (kill-new (car (get-text-property (point) 'erc-data))))
   ("w"   (wget (car (get-text-property (point) 'erc-data))))))

(with-eval-after-load 'erc-list
  (al/bind-keys
   :map erc-list-menu-mode-map
   ("u"   . erc-list-join)
   ("RET" . erc-list-join))
  (define-key erc-list-menu-sort-button-map
    [header-line mouse-2] 'erc-list-menu-sort-by-column))

(with-eval-after-load 'al-erc
  (setq
   al/erc-log-excluded-regexps
   '("\\`#archlinux\\'" "\\`#emacs\\'" "\\`#freenode\\'" "\\`#znc\\'")
   al/erc-away-msg-list
   '("just away" "watching athletics" "watching darts"
     "eating" "i'm not ready to chat" "time to sleep")
   al/erc-channel-list
   '("#emacs" "#archlinux" "#archlinux-classroom" "#trivialand" "##latin"
     "#lisp" "#lispgames" "#git" "#github" "#netfilter" "#wesnoth"
     "#themanaworld" "##french" "##english" "##programming")))

(al/autoload "erc-view-log" erc-view-log-mode)
(al/with-check
  :var 'erc-log-channels-directory
  (push (cons (concat "\\`"
                      (regexp-quote (expand-file-name
                                     erc-log-channels-directory)))
              'erc-view-log-mode)
        auto-mode-alist))


;;; Debbugs

(al/bind-keys
 :prefix-map al/debbugs-map
 :prefix-docstring "Map for debbugs."
 :prefix "M-B"
 ("M-B" . debbugs-gnu)
 ("n"   . debbugs-gnu-bugs)
 ("b"     (switch-to-buffer "*Guix-Patches Bugs*"))
 ("s"   . debbugs-gnu-search))

(with-eval-after-load 'debbugs-gnu
  (setq debbugs-gnu-default-packages '("guix-patches"))
  (push "guix-patches" debbugs-gnu-all-packages)

  (defconst al/debbugs-gnu-keys
    '(("u" . debbugs-gnu-select-report))
    "Alist of auxiliary keys for `debbugs-gnu-mode-map'.")
  (al/bind-keys-from-vars 'debbugs-gnu-mode-map 'al/debbugs-gnu-keys))


;;; Misc settings and packages

(with-eval-after-load 'url
  (setq url-configuration-directory (al/emacs-data-dir-file "url")))

(with-eval-after-load 'wget
  (setq
   wget-debug-buffer "*wget-log*"
   wget-download-directory-filter 'wget-download-dir-filter-regexp
   wget-download-log-file (al/emacs-data-dir-file "emacs-wget.log")
   wget-download-directory
   `(("onlinetv" . ,(al/download-dir-file "onlinetv"))
     ("beatles" . ,(al/echo-download-dir-file "beatles"))
     ("classicrock" . ,(al/echo-download-dir-file "classicrock"))
     (,(regexp-quote "echo.msk.ru") . ,al/echo-download-dir)
     ("." . ,al/download-dir))))

(with-eval-after-load 'mentor
  (setq mentor-rtorrent-url "scgi://127.0.0.1:5000"))

(with-eval-after-load 'net-utils
  (setq ping-program-options '("-c" "3")))

(with-eval-after-load 'al-net
  (setq
   al/net-hosts '("zeus" "hyperion" "192.168.1.1" "10.11.149.1"
                   "10.10.0.1" "google.com" "ya.ru")
   al/router-log-path "~/docs/net/RT_G32.log/"))

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

(with-eval-after-load 'debpaste
  (setq
   debpaste-user-name "alezost"
   debpaste-expire-time (* 3 24 60 60))
  (add-to-list 'debpaste-domains "debpaste" t))

(al/autoload "web-search"
  web-search-yandex
  web-search-ipduh
  web-search-ip-address
  web-search-wikipedia-ru
  web-search-arch-package
  web-search-multitran
  web-search-ej)

(with-eval-after-load 'web-search
  :config
  (web-search-add-engine
   'ipduh "IPduh"
   "http://ipduh.com/apropos/?%s"
   'web-search-clean-ip)
  (web-search-add-engine
   'ip-address "IP address"
   "http://www.ip-address.org/lookup/ip-locator.php?track=%s"
   'web-search-clean-ip)
  (web-search-add-engine
   'yandex "Yandex"
   "http://yandex.ru/yandsearch?text=%s")
  (web-search-add-engine
   'wikipedia-en "Wikipedia (english)"
   "http://en.wikipedia.org/w/index.php?search=%s")
  (web-search-add-engine
   'wikipedia-ru "Wikipedia (russian)"
   "http://ru.wikipedia.org/w/index.php?search=%s")
  (web-search-add-engine
   'arch-package "Arch Packages"
   "https://www.archlinux.org/packages/?sort=&q=%s&maintainer=&flagged=")
  (web-search-add-engine
   'multitran "Multitran"
   "http://www.multitran.ru/c/M.exe?CL=1&s=%s")
  (web-search-add-engine
   'ej "ej.ru"
   "http://mvvc44tv.cmle.ru/?a=note&id=%s"))

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

;;; net.el ends here
