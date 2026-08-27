;;; aurel.el --- Settings for `aurel' package  -*- lexical-binding: t -*-

(require 'aurel)
(require 'al-key)

(setq
 aurel-download-directory (al/src-dir-file "abs")
 aurel-aur-user-name "alezost"
 ;; aurel-aur-user-package-info-check t
 aurel-info-aur-user-string "——————————————————————————————————————\n"
 aurel-info-installed-package-string aurel-info-aur-user-string
 aurel-debug-level 9
 aurel-list-format
 '((name aurel-list-get-name 20 t)
   (version nil 9 t)
   (installed-version nil 9 t)
   (maintainer aurel-list-get-maintainer 13 t)
   (votes nil 8 bui-list-sort-numerically-4 :right-align t)
   (description nil 30 nil))
 aurel-list-titles
 '((votes . "V.")
   (version . "Ver.")
   (installed-version . "Inst.")))

(al/bind-keys
  :map aurel-filter-map
  ;; Default `aurel-enable-filter' was left for backward compatibility.
  ("f" 'bui-enable-filter))

;;; aurel.el ends here
