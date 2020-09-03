;;; al-minibuffer.el --- Additional functionality for minibuffer

;; Copyright © 2013–2017, 2020 Alex Kost

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

;; Idea to use a custom completing-read function with the possibility to
;; fallback to `completing-read-default' came from
;; <http://www.emacswiki.org/emacs/InteractivelyDoThings#toc15>.

(require 'cl-lib)

(defvar al/completing-read-engine
  (if (boundp 'ivy-mode)
      'ivy
    'ido)
  "Engine used by `al/completing-read'.
Can be either `ivy', `ido' or nil (to fallback to
`completing-read-default').")

(declare-function ivy-completing-read "ivy" t)

(defun al/completing-read (prompt collection &optional predicate
                                  require-match initial-input
                                  hist def inherit-input-method)
  "Function for `completing-read-function' variable.
Use completion engine depending on `al/completing-read-engine'."
  (let ((input-method (and inherit-input-method
                           current-input-method)))
    ;; Currently, `ivy' and `ido' do not provide a way to handle
    ;; input-method: both `ivy-completing-read' and
    ;; `ido-completing-read' simply ignore `inherit-input-method'
    ;; argument.  So, `minibuffer-setup-hook' need to be adjusted.
    (minibuffer-with-setup-hook
        (lambda () (set-input-method input-method))
      ;; Match is never required in the following calls, otherwise it's not
      ;; possible to select "#XXXXXX" with `read-color'.
      (cl-case al/completing-read-engine
        (ivy
         (ivy-completing-read prompt collection predicate
                              nil initial-input
                              hist def inherit-input-method))
        (ido
         (ido-completing-read prompt (all-completions "" collection predicate)
                              nil nil initial-input
                              hist def inherit-input-method))
        (t
         ;; `minibuffer-complete' (bound to TAB in minibuffer prompt) calls
         ;; `completion-in-region', so return
         ;; `completion-in-region-function' to default value (in particular,
         ;; ivy changes it).
         (let ((completion-in-region-function 'completion--in-region))
           (completing-read-default prompt collection predicate
                                    nil initial-input
                                    hist def inherit-input-method)))))))

(defun al/complete-default (fun &rest args)
  "Use `completing-read-default' for FUN.
This function is intended to be used as an 'around' advice for
FUN, for example:

  (advice-add 'org-set-tags :around #'al/complete-default)"
  (let (al/completing-read-engine)
    (apply fun args)))

(provide 'al-minibuffer)

;;; al-minibuffer.el ends here
