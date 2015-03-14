;;; init.el --- Init file   -*- lexical-binding: t -*-

;; Copyright © 2012-2015 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Location of various files

(defmacro al/file-accessors (name val)
  "Define variable and function for accessing my directories."
  (let ((dir-var  (intern (concat "al/" name "-dir")))
        (file-fun (intern (concat "al/" name "-dir-file"))))
    `(progn
       (defvar ,dir-var ,val)
       (defun ,file-fun (file)
         ,(format "Return full file name of a FILE placed in `%s'."
                  dir-var)
         (expand-file-name file ,dir-var)))))

(al/file-accessors "emacs-init"
                    (file-name-directory
                     (file-truename load-file-name)))
(al/file-accessors "emacs"         (al/emacs-init-dir-file "../"))
(al/file-accessors "emacs-data"    (al/emacs-dir-file "data"))
(al/file-accessors "gnus"          (al/emacs-data-dir-file "gnus"))
(al/file-accessors "gnus-news"     (al/gnus-dir-file "news"))
(al/file-accessors "gnus-mail"     (al/gnus-dir-file "mail"))

(al/file-accessors "config"        "~/config")
(al/file-accessors "notes"         "~/notes")
(al/file-accessors "progs"         "~/progs")
(al/file-accessors "journal"       (al/notes-dir-file  "journal"))
(al/file-accessors "music"         "~/music")
(al/file-accessors "sound"         "~/docs/audio/small")
(al/file-accessors "tmp"           "~/temp")
(al/file-accessors "src"           "~/src")
(al/file-accessors "download"      "~/downloads")
(al/file-accessors "echo-download" (al/download-dir-file "echo"))

(al/file-accessors "guix-user-profile" "~/.guix-profile")
(al/file-accessors "guix-system-profile" "/run/current-system/profile")


;;; Auxiliary functions and macros

(defun al/warning (format-string &rest args)
  "Display a warning message."
  (apply #'message
         (concat "WARNING: " format-string)
         args))

(defun al/p (predicate val &optional message)
  "Return non-nil if PREDICATE returns non-nil on VAL.
Otherwise display warning MESSAGE on VAL and return nil."
  (or (funcall predicate val)
      (progn (and message (al/warning message val))
             nil)))

(defun al/every (predicate vals &optional message)
  "Return non-nil if PREDICATE returns non-nil on each element of VALS.
If VALS is not a list, call PREDICATE on this value."
  (if (and (listp vals)
           (not (functionp vals))) ; to avoid treating "(lambda …)" as list
      (cl-every (lambda (val)
                  (al/p predicate val message))
                vals)
    (al/p predicate vals message)))

(defun al/function? (object)
  "Non-nil if OBJECT is a function or a list of functions."
  (al/every #'functionp object
            "Unknown function '%S'."))

(defun al/bound? (object)
  "Non-nil if OBJECT is a bound symbol or a list of bound symbols."
  (al/every #'boundp object
            "Symbol '%S' is not bound."))

(defun al/file? (object)
  "Non-nil if OBJECT is an existing file or a list of directories."
  (al/every #'file-exists-p object
            "File '%s' does not exist."))

(defun al/directory? (object)
  "Non-nil if OBJECT is an existing directory or a list of directories."
  (al/every #'file-directory-p object
            "Directory '%s' does not exist."))

(defmacro al/with-check (&rest body)
  "Call rest of BODY if all checks are passed successfully.

BODY should start with checks (keyword arguments).  The following
keywords are available: `:fun'/`:var'/`:file'/`:dir'.  Each
keyword argument may be an object or a list of objects.  These
objects are checkced to be a proper function / a bound symbol /
an existing file / an existing directory.

Return nil if checks are not passed."
  (declare (indent 0) (debug (name body)))
  (let (fun var file dir)

    (while (keywordp (car body))
      (pcase (pop body)
        (`:fun  (setq fun  (pop body)))
        (`:var  (setq var  (pop body)))
	(`:file (setq file (pop body)))
	(`:dir  (setq dir  (pop body)))
	(_ (pop body))))

    `(when (and ,(or (null fun)  `(al/function?  ,fun))
                ,(or (null var)  `(al/bound?     ,var))
                ,(or (null file) `(al/file?      ,file))
                ,(or (null dir)  `(al/directory? ,dir)))
       ,@body)))

(defun al/funcall-or-dolist (val function)
  "Call FUNCTION on VAL if VAL is not a list.
If VAL is a list, call FUNCTION on each element of the list."
  (declare (indent 1))
  (if (listp val)
      (dolist (v val)
        (funcall function v))
    (funcall function val)))

(defun al/list-maybe (obj)
  "Return OBJ if it is a list, or a list with OBJ otherwise."
  (if (listp obj) obj (list obj)))

(defun al/add-to-load-path-maybe (&rest dirs)
  "Add existing directories from DIRS to `load-path'."
  (dolist (dir dirs)
    (al/with-check
      :dir dir
      (push dir load-path))))

(defun al/add-my-package-to-load-path-maybe (name)
  "Add directory with my package NAME (if it exists) to `load-path'."
  (al/add-to-load-path-maybe (al/emacs-dir-file name)))

(defun al/load (file)
  "Load FILE.
FILE may omit an extension.  See `load' for details."
  (or (load file 'noerror)
      (al/warning "Failed to load '%s'." file)))

(defun al/init-load (file)
  "Load FILE from `al/emacs-init-dir'."
  (al/load (al/emacs-init-dir-file file)))

(defun al/add-hook-maybe (hooks functions &optional append local)
  "Add all bound FUNCTIONS to all HOOKS.
Both HOOKS and FUNCTIONS may be single variables or lists of those."
  (declare (indent 1))
  (al/funcall-or-dolist functions
    (lambda (fun)
      (al/with-check
        :fun fun
        (al/funcall-or-dolist hooks
          (lambda (hook)
            (add-hook hook fun append local)))))))

(defun al/clean-map (map-var)
  "Remove all key bindings from MAP-VAR variable."
  (al/with-check
    :var map-var
    (setcdr (symbol-value map-var) nil)))

(defun al/modify-page-break-syntax (table-var)
  "Set non-whitespace syntax for ^L in syntax table from TABLE-VAR.
Page break should not belong to whitespace syntax, because
`back-to-indentation' moves the point after ^L character which is not good.
Also it (default syntax) breaks `indent-guide-mode'."
  (al/with-check
    :var table-var
    (modify-syntax-entry ?\f ">   " (symbol-value table-var))))

(defsubst al/emacs-trunk-p ()
  "Return non-nil, if current Emacs is the latest development build."
  (not (version< emacs-version "24.4.50")))

(defun al/guix-system-p ()
  "Return non-nil, if current OS is GuixSD."
  (file-exists-p al/guix-system-profile-dir))

(defun al/show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(defun al/no-truncate-lines ()
  (setq-local truncate-lines nil))

(defun al/inhibit-field-motion ()
  (setq-local inhibit-field-text-motion t))


;;; External packages

(setq load-prefer-newer t)
(setq
 quelpa-upgrade-p t
 ;; quelpa dirs are used in several places of my config.
 quelpa-dir (expand-file-name "quelpa" user-emacs-directory)
 quelpa-build-dir (expand-file-name "build" quelpa-dir))

(package-initialize)

(defvar al/fresh-init (not (fboundp 'quelpa))
  "Non-nil, if this is the first time my config is loaded by Emacs.")

(defun al/gitorious-repo (name)
  "Return git url of my gitorious repository NAME."
  (concat "git://gitorious.org/alezost-emacs/" name ".git"))

(defvar al/main-packages
  `((use+bind           :fetcher github :repo "jwiegley/use-package"
                        :files ("bind-key.el" "use-package.el"))
    (quelpa             :fetcher github :repo "quelpa/quelpa")
    (utils              :fetcher git :url ,(al/gitorious-repo "utils"))
    (dvorak-layouts     :fetcher git :url ,(al/gitorious-repo "dvorak-layouts"))
    (alect-themes       :fetcher git :url ,(al/gitorious-repo "alect-themes"))
    (mwim               :fetcher git :url ,(al/gitorious-repo "mwim"))
    (insert-pair        :fetcher git :url ,(al/gitorious-repo "insert-pair"))
    (imenus             :fetcher git :url ,(al/gitorious-repo "imenus"))
    smex
    paredit
    elisp-slime-nav
    diminish
    (yasnippet          :fetcher github :repo "capitaomorte/yasnippet"
                        :files ("yasnippet.el"))
    (magit              :fetcher github :repo "magit/magit"
                        :files ("*.el" "*.texi"))
    github-browse-file
    (xml-rpc            :fetcher github :repo "emacsmirror/xml-rpc")
    (point-pos          :fetcher git :url ,(al/gitorious-repo "point-pos"))
    (web-search         :fetcher git :url ,(al/gitorious-repo "web-search"))
    (text-search        :fetcher git :url ,(al/gitorious-repo "text-search"))
    (echo-msk           :fetcher git :url ,(al/gitorious-repo "echo-msk"))
    (darts-value        :fetcher git :url ,(al/gitorious-repo "darts-value"))
    (debpaste           :fetcher git :url ,(al/gitorious-repo "debpaste"))
    (aurel              :fetcher git :url ,(al/gitorious-repo "aurel"))
    (make-color         :fetcher git :url ,(al/gitorious-repo "make-color"))
    (pretty-sha-path    :fetcher git :url ,(al/gitorious-repo "pretty-sha-path"))
    (date-at-point      :fetcher git :url ,(al/gitorious-repo "date-at-point"))
    (journal            :fetcher git :url ,(al/gitorious-repo "journal"))
    (mana               :fetcher git :url ,(al/gitorious-repo "mana"))
    (ducpel             :fetcher git :url ,(al/gitorious-repo "ducpel")
                        :files ("*.el"))
    pdf-tools
    org-pdfview
    (dictem             :fetcher github :repo "cheusov/dictem")
    google-translate
    geiser
    emms
    (emms-player-mpv    :fetcher github :repo "dochang/emms-player-mpv")
    browse-kill-ring
    outline-magic
    markdown-mode
    syslog-mode
    pkgbuild-mode
    mentor
    sauron
    (sunrise-commander  :fetcher github :repo "escherdragon/sunrise-commander")
    erc-hl-nicks
    (erc-view-log       :fetcher github :repo "alezost/erc-view-log"
                        :branch "general-regexps")
    indent-guide
    hl-todo))

(defvar al/additional-packages
  '(slime
    (mysql              :fetcher github :repo "haxney/mysql")
    (sql-completion     :fetcher github :repo "emacsmirror/sql-completion")
    (rainbow-mode       :fetcher url :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el")
    (typing-practice    :fetcher url :url "https://raw.github.com/mebubo/dotfiles/master/.emacs.d/site-lisp/typing-practice.el")
    (sokoban            :fetcher github :repo "leoliu/sokoban"
                        :files ("*.el" "sokoban.levels"))))

(defun al/all-packages ()
  "Return all package recipes I use."
  (append al/main-packages al/additional-packages))

(defun al/package-name (name-or-recipe)
  "Return package name (symbol) by NAME-OR-RECIPE."
  (if (listp name-or-recipe)
      (car name-or-recipe)
    name-or-recipe))

(defun al/package-recipe (name-or-recipe)
  "Return package recipe by NAME-OR-RECIPE."
  (if (listp name-or-recipe)
      name-or-recipe
    (cl-find-if (lambda (recipe)
                  (eq name-or-recipe (al/package-name recipe)))
                (al/all-packages))))

(defun al/read-package-name ()
  "Prompt for and return a package name (symbol)."
  (let ((names (mapcar (lambda (recipe)
                         (symbol-name (al/package-name recipe)))
                       (al/all-packages))))
    (intern (completing-read "Update/install: " names nil t))))

(defun al/quelpa (&rest recipes)
  "Install/update packages using RECIPES.

Each recipe from RECIPES should be either a MELPA package
name (symbol) or a full recipe (list).

Interactively, prompt for a package to update/install.

With \\[universal-argument], update all packages from `al/main-packages'.

With \\[universal-argument] \\[universal-argument], \
update also the packages from `al/additional-packages'."
  (interactive
   (cond ((equal current-prefix-arg '(4))
          al/main-packages)
         ((equal current-prefix-arg '(16))
          (al/all-packages))
         (t (list (al/package-recipe (al/read-package-name))))))
  (mapc #'quelpa recipes))

(defun al/package-installed-p (fun package &rest args)
  "Do not check the version of a built-in package.
Some built-in packages (e.g., `org', `erc') do not have 'Version'
header field.  This may break things if a third-party package
relies on a particular version of a built-in package (e.g.,
'org-6.1' or 'erc-5.3').  So just ignore the version."
  (or (package-built-in-p package)
      (apply fun package args)))

(advice-add 'package-installed-p :around #'al/package-installed-p)


;;; Loading the rest config and required packages

;; If this is the first start of emacs, bootstrap quelpa and install all
;; the packages.
(when al/fresh-init
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer))
  ;; My 'utils' package requires everything, so it should be compiled
  ;; (i.e. installed) after all.
  (apply #'al/quelpa
         (append (cl-remove-if (lambda (recipe)
                                 (eq 'utils (al/package-name recipe)))
                               (al/all-packages))
                 (list (al/package-recipe 'utils)))))

(require 'bind-key)
(setq bind-key-describe-special-forms t)

(require 'use-package)
(setq use-package-verbose t)

(al/add-my-package-to-load-path-maybe "utils")

(al/init-load "keys")
(al/init-load "text")
(al/init-load "settings")
(al/init-load "files")
(al/init-load "prog")
(al/init-load "time")
(al/init-load "file-modes")
(al/init-load "mmedia")
(al/init-load "packages")
(al/init-load "net")
(al/init-load "dict")
(al/init-load "visual")
(al/init-load "games")

;; (setq custom-file "/tmp/custom.el")
(setq custom-file (al/emacs-init-dir-file "custom.el"))
(add-hook 'after-init-hook (lambda () (load custom-file 'noerror)))

;;; init.el ends here
