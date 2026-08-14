;;; gnus.el --- Settings for `gnus' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'gnus)
(require 'al-places)
(require 'al-key)
(require 'al-gnus)

(declare-function al/gnus-dir-file "net")
(defvar al/mail-user-name)

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

 gnus-summary-line-format "%U%R%z %(%&user-date; %B%-3L %[%f%]%) %s\n"
 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
 gnus-activate-level 3
 gnus-activate-foreign-newsgroups gnus-activate-level
 gnus-large-newsgroup 400

 al/atom2rss-file (al/emacs-data-dir-file "atom2rss.xsl"))

(al/call-at-hook dired-mode-hook turn-on-gnus-dired-mode)

(advice-add 'mm-url-insert
  :after #'al/convert-atom-to-rss)
(advice-add 'gnus-agent-make-mode-line-string
  :around #'al/gnus-agent-mode-line-string)

(al/eval-after-load nntp
  (setq nntp-connection-timeout 10))

(al/eval-after-load gnus-msg
  (setq gnus-gcc-mark-as-read t))

(al/eval-after-load mm-decode
  (setq
   mm-text-html-renderer 'gnus-w3m
   mm-discouraged-alternatives '("text/html" "text/richtext")))

(al/eval-after-load gnus-srvr
  (defconst al/gnus-server-keys
    '(("u"   . gnus-server-read-server)
      ("M-d" . gnus-server-edit-server)))
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
  (setq
   gnus-group-goto-unread nil
   gnus-group-buffer "*Gnus Groups*"
   gnus-group-mode-line-format "Gnus:"
   gnus-group-goto-unread nil)

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

  (al/call-at-hook gnus-group-mode-hook
    gnus-topic-mode
    hl-line-mode))

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
    '("x" "M-r"
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
      ("w"       (wget (al/gnus-summary-find-mm-url)))))
  (al/bind-keys-from-vars 'gnus-summary-mode-map 'al/gnus-summary-keys)

  (al/bind-keys
    :map gnus-summary-mode-map
    :prefix-map al/gnus-summary-search-map
    :prefix-docstring "Search map for `gnus-summary-mode'."
    :prefix "M-s"
    ("M-s" . gnus-summary-search-article-forward)
    ("M-r" . gnus-summary-search-article-backward))

  (al/call-at-hook gnus-summary-mode-hook
    al/hbar-cursor-type
    hl-line-mode))

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
    '("C-d"))
  (defconst al/gnus-url-button-keys
    '(("c" . gnus-article-copy-string)))
  (defconst al/gnus-mime-button-keys
    '(("u" . gnus-mime-action-on-part)
      ("s" . gnus-mime-save-part)
      ("v" . gnus-mime-view-part-internally)
      ("V" . gnus-mime-view-part)))

  (al/bind-keys-from-vars 'gnus-article-mode-map
    '(al/button-keys al/gnus-article-keys))
  (al/bind-keys-from-vars 'gnus-url-button-map
    '(al/button-keys al/gnus-url-button-keys))
  (al/bind-keys-from-vars 'gnus-mime-button-map
    '(al/button-keys al/gnus-mime-button-keys))

  ;; Wrap text in gnus-article buffers by words.
  (add-hook 'gnus-article-mode-hook #'visual-line-mode))

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
      ("C-c P" . mml-preview)))
  (al/bind-keys-from-vars 'mml-mode-map 'al/mml-keys))

;;; gnus.el ends here
