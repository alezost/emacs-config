;;; al-quail-hangul.el --- Modified Hangul input method  -*- lexical-binding: t -*-

;; Copyright © 2024 Alex Kost

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

;;; Commentary:

;; This file provides an alternative Hangul keymap (convenient for me).

;;; Code:

;; XXX quail libraries are not in `load-path'.  Is there a better way to
;; require `hangul' at compile time?
(eval-when-compile
  (load "leim/quail/hangul"))

(require 'hangul)

;; Original keymap taken from `hangul2-keymap' by `hangul2-input-method-internal'.
;;  a  b  c  d  e f g  h  i  j  k  l  m  n  o  p  q  r s t  u  v  w  x  y  z  E O  P  Q  R T  W
;; [17 48 26 23 7 9 30 39 33 35 31 51 49 44 32 36 18 1 4 21 37 29 24 28 43 27 8 34 38 19 2 22 25]

(defvar al/quail-hangul-keys
  [
   ;; A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
      33 19 23 8  38 34 2  30 51 25 2  9  17 4  43 19 25 37 22 8  48 0  0  0  49 0
   ;; [ \ ] ^ _ `
      0 0 0 0 0 0
   ;; a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
      31 18 23 7  36 32 1  30 51 24 27 9  17 4  39 29 26 35 21 28 44 0  0  0  49 0
  ])

(defun al/quail-hangul2-input-method-internal (key)
  "Replacement for `hangul2-input-method-internal'."
  (let ((char (aref al/quail-hangul-keys (- key 65))))
    (unless (= char 0)
      (if (< char 31)
          (hangul2-input-method-jaum char)
        (hangul2-input-method-moum char)))))

(provide 'al-quail-hangul)

;;; al-quail-hangul.el ends here
