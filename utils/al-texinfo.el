;;; al-texinfo.el --- Additional functionality for texinfo

;; Copyright © 2016 Alex Kost

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

;;; Code:

(require 'texinfo)

(define-skeleton al/texinfo-insert-@menu
  "Insert `@menu' clause in a Texinfo buffer."
  nil
  "@menu\n" _ "@end menu\n")

(define-skeleton al/texinfo-insert-@example
  "Insert `@example' clause in a Texinfo buffer."
  nil
  "@example\n" _ "@end example\n")

(define-skeleton al/texinfo-insert-@itemize
  "Insert `@itemize' clause in a Texinfo buffer."
  nil
  "@itemize " _ "\n@end itemize\n")

(define-skeleton al/texinfo-insert-@table
  "Insert `@table' clause in a Texinfo buffer."
  nil
  "@table " _ "\n@end table\n")

(define-skeleton al/texinfo-insert-@deffn
  "Insert `@deffn' clause in a Texinfo buffer."
  nil
  "@deffn " _ "\n@end deffn\n")

(provide 'al-texinfo)

;;; al-texinfo.el ends here
