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

(eval-when-compile
  (require 'al-aux-macros))
(require 'al-places)
(require 'al-general)
(require 'al-key)

(declare-function al/gnus-dir-file "net.el")


;;; Global keys

(al/bind-key* "M-S" tui/web-search)

(al/bind-keys
 :prefix-map al/net-map
 :prefix-doc "Map for net utils."
 :prefix-key "C-w"
 ("p" . al/ping)
 ("t" . al/traceroute)
 ("w" . wget)
 ("m" . al/url-wget-mp3))


;;; Browsing

(al/eval-after-load w3m
  (al/load-settings "w3m"))

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
  :depth 90
  (al/file-accessors "gnus"
    (al/emacs-data-dir-file (concat "gnus-" al/mail-user-name)))
  (al/setq-no-warnings
   ;; Set `gnus-home-directory' before loading Gnus.  Otherwise,
   ;; `gnus-startup-file' will be set to "~/.newsrc" for some reason.
   gnus-home-directory al/gnus-dir
   user-mail-address (concat al/mail-user-name "@gmail.com")))

(al/bind-keys
 :prefix-map al/gnus-map
 :prefix-doc "Map for Gnus."
 :prefix-key "M-g"
 ("M-g" . al/gnus-switch-win-config)
 ("g"   . al/gnus-switch-to-group-buffer)
 ("b"   . al/gnus-switch-buffer)
 ("m"   . gnus-msg-mail)
 ("n"   . gnus-msg-mail))

(al/eval-after-load gnus
  (al/load-settings "gnus"))

(al/eval-after-load sendmail
  (setq
   send-mail-function 'smtpmail-send-it))

(al/eval-after-load smtpmail
  (setq
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587))

(al/eval-after-load shr
  (al/bind-keys
   :map shr-map
   ("u" . shr-browse-url)
   ("c" . shr-copy-url)))


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
   truncate))

;; Set it here (originally defined at `erc-log') to use below.
(defvar erc-log-channels-directory
  (al/emacs-data-dir-file "erc-log"))

(al/bind-keys*
 :prefix-map al/erc-map
 :prefix-doc "Map for ERC."
 :prefix-key "M-c"
 ("M-c" . al/erc-track-switch-buffer)
 ("M-n" . al/erc-cycle)
 ("l"   . al/erc-channel-list)
 ("b"   . al/erc-switch-buffer)
 ("M-s" . al/erc-switch-to-server-buffer)
 ;; Non-interactive `erc' - compute everything without prompting:
 ("c"     (erc))
 ("R"   . al/erc-server-buffer-rename)
 ("d"   . al/erc-quit-server)
 ("j"   . al/erc-join-channel)
 ("a"   . al/erc-away)
 ("u"   . al/erc-number-of-users)
 ("m"   . erc-track-mode)
 ("n"   . erc-notifications-mode)
 ("e"     (al/display-buffer "#emacs"))
 ("x"     (al/display-buffer "#guix"))
 ("s"     (al/display-buffer "#stumpwm"))
 ("M-z"   (al/display-buffer "*status")))

(al/eval-after-load erc
  (al/load-settings "erc"))

;; TODO use `al/add-to-auto-mode-alist'
(push (cons (concat "\\`"
                    (regexp-quote (expand-file-name
                                   erc-log-channels-directory)))
            'erc-view-log-mode)
      auto-mode-alist)

(al/eval-after-load erc-view-log
  (setq
   erc-view-log-timestamp-regexp
   (rx "[" (one-or-more (or digit ":")) "]")
   erc-view-log-timestamp-position 'left
   ))


;;; Debbugs

(al/bind-keys
 :prefix-map al/debbugs-map
 :prefix-doc "Map for debbugs."
 :prefix-key "M-B"
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
 :prefix-doc "Map for debpaste."
 :prefix-key "C-H-p"
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
