;;; al-quail.el --- Additional input methods

;; Copyright © 2019–2020 Alex Kost

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

;; This file provides additional input methods that can be used with
;; `set-input-method' and `toggle-input-method' commands.

;;; Code:

(require 'quail)

;; Originates from "latin-prefix" input method.
(quail-define-package
 "al/latin-prefix" "Latin" "ä" t
 "Latin-1 characters input method with prefix modifiers

| effect         | prefix | examples       |
|----------------+--------+----------------|
| acute, cedilla | \\='      | \\='a → á, \\='c → ç |
| diaeresis      | ;      | ;a → ä, ;s → ß |
| grave          | \\=`      | \\=`a → à         |
| tilde, caron   | ~      | ~a → ã, ~c → č |
| circumflex     | ^      | ^a → â         |
| superscript    | ^      | ^1 → ¹, ^n → ⁿ |
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'a" ?á)
 ("'A" ?Á)
 ("'c" ?ç)
 ("'C" ?Ç)
 ("'e" ?é)
 ("'E" ?É)
 ("'i" ?í)
 ("'I" ?Í)
 ("'o" ?ó)
 ("'O" ?Ó)
 ("'u" ?ú)
 ("'U" ?Ú)
 ("'w" ?ẃ)
 ("'W" ?Ẃ)
 ("'y" ?ý)
 ("'Y" ?Ý)

 (";a" ?ä)
 (";A" ?Ä)
 (";e" ?ë)
 (";E" ?Ë)
 (";i" ?ï)
 (";I" ?Ï)
 (";o" ?ö)
 (";O" ?Ö)
 (";s" ?ß)
 (";u" ?ü)
 (";U" ?Ü)
 (";w" ?ẅ)
 (";W" ?Ẅ)
 (";y" ?ÿ)
 (";Y" ?Ÿ)

 ("`0" ?₀)
 ("`1" ?₁)
 ("`2" ?₂)
 ("`3" ?₃)
 ("`4" ?₄)
 ("`5" ?₅)
 ("`6" ?₆)
 ("`7" ?₇)
 ("`8" ?₈)
 ("`9" ?₉)
 ("`-" ?₋)
 ("`+" ?₊)
 ("`=" ?₌)
 ("`k" ?ₖ)
 ("`l" ?ₗ)
 ("`m" ?ₘ)
 ("`n" ?ₙ)
 ("`p" ?ₚ)
 ("`s" ?ₛ)
 ("`t" ?ₜ)
 ("`A" ?À)
 ("`E" ?È)
 ("`I" ?Ì)
 ("`O" ?Ò)
 ("`U" ?Ù)
 ("`W" ?Ẁ)
 ("`Y" ?Ỳ)
 ("`a" ?à)
 ("`e" ?è)
 ("`i" ?ì)
 ("`o" ?ò)
 ("`u" ?ù)
 ("`w" ?ẁ)
 ("`y" ?ỳ)

 ("~p" ?£)
 ("~e" ?€)
 ("~r" ?₽)
 ("~a" ?ã)
 ("~A" ?Ã)
 ("~c" ?č)
 ("~C" ?Č)
 ("~d" ?ď)
 ("~D" ?Ď)
 ("~e" ?ě)
 ("~E" ?Ě)
 ("~g" ?ğ)
 ("~G" ?Ğ)
 ("~n" ?ñ)
 ("~N" ?Ñ)
 ("~o" ?õ)
 ("~O" ?Õ)
 ("~s" ?š)
 ("~S" ?Š)
 ("~t" ?þ)
 ("~T" ?Þ)
 ("~u" ?ŭ)
 ("~U" ?Ŭ)
 ("~z" ?ž)
 ("~Z" ?Ž)

 ("^0" ?⁰)
 ("^1" ?¹)
 ("^2" ?²)
 ("^3" ?³)
 ("^4" ?⁴)
 ("^5" ?⁵)
 ("^6" ?⁶)
 ("^7" ?⁷)
 ("^8" ?⁸)
 ("^9" ?⁹)
 ("^_" ?¯)
 ("^-" ?⁻)
 ("^+" ?⁺)
 ("^=" ?⁼)
 ("^n" ?ⁿ)
 ("^a" ?â)
 ("^A" ?Â)
 ("^c" ?ĉ)
 ("^C" ?Ĉ)
 ("^e" ?ê)
 ("^E" ?Ê)
 ("^g" ?ĝ)
 ("^G" ?Ĝ)
 ("^h" ?ĥ)
 ("^H" ?Ĥ)
 ("^i" ?î)
 ("^I" ?Î)
 ("^j" ?ĵ)
 ("^J" ?Ĵ)
 ("^o" ?ô)
 ("^O" ?Ô)
 ("^s" ?ŝ)
 ("^S" ?Ŝ)
 ("^u" ?û)
 ("^U" ?Û)
 ("^w" ?ŵ)
 ("^W" ?Ŵ)
 ("^Y" ?Ŷ)
 ("^y" ?ŷ))

(provide 'al-quail)

;;; al-quail.el ends here
