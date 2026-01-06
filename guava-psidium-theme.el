;;; guava-psidium-theme.el --- An Emacs theme purely inspired by guava colors. -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 07, 2025
;; Version: 0.4.0
;; Keywords: themes, faces, color
;; URL: http://github.com/bormoge/guava-themes.el
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;; Commentary:
;;
;; A theme inspired by guava colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-psidium "A theme inspired by guava colors.")

(let* (
      (guava-psidium-class '((class color) (min-colors 257)))
      (guava-psidium-black             "#000000")

      (guava-psidium-white             "#FFFFFF")
      (guava-psidium-cream             "#F1EECE");F7DEB6

      (guava-psidium-green             "#599F48")
      (guava-psidium-oceanic-green     "#3ab992");13765e,13845e,139570,3ab488
      (guava-psidium-guava-green       "#AED734");AECA41,AED234
      (guava-psidium-deep-green        "#097d2c");09662c

      (guava-psidium-orange            "#FF7D5F")
      (guava-psidium-deep-orange       "#D43D1A")
      (guava-psidium-red               "#f02d1b")
      (guava-psidium-light-pink        "#FCD0C9");F8917C
      (guava-psidium-pink              "#F8767C");F84865,F8767C,F88686,F85A65

      (guava-psidium-light-brown       "#735944")
      (guava-psidium-brown             "#7D5E45")

      (guava-psidium-light-blue        "#41C3CA")
      (guava-psidium-blue              "#008B8B");245feb
      (guava-psidium-deep-blue         "#483d8b");004F5D
      (guava-psidium-antarctic-blue    "#8d76ca");6a5997

      (guava-psidium-light-purple      "#a62d90");D7137C,C0137C,B00CE0

      (guava-psidium-error             "#FF0000")
      (guava-psidium-warning           "#F68511");FF8C00,f08020,f68511
      (guava-psidium-success           "#228B22")

      (guava-psidium-vc-change         guava-psidium-light-blue)
      (guava-psidium-vc-insert         guava-psidium-green)
      (guava-psidium-vc-delete         guava-psidium-orange)
      )

  (custom-theme-set-faces
   'guava-psidium

   ;; default for guava-psidium
   `(default ((,guava-psidium-class (:foreground ,guava-psidium-black :background ,guava-psidium-cream))))

   ;; error, warning, success
   `(error ((,guava-psidium-class (:foreground ,guava-psidium-error :weight bold))))
   `(warning ((,guava-psidium-class (:foreground ,guava-psidium-warning :weight bold))))
   `(success ((,guava-psidium-class (:foreground ,guava-psidium-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-psidium-class (:background ,guava-psidium-green :foreground ,guava-psidium-white))))

   ;; fringe
   `(fringe ((,guava-psidium-class (:background ,guava-psidium-cream :foreground ,guava-psidium-cream))))
   `(diff-hl-change ((,guava-psidium-class (:background ,guava-psidium-vc-change :foreground ,guava-psidium-vc-change))))
   `(diff-hl-insert ((,guava-psidium-class (:background ,guava-psidium-vc-insert :foreground ,guava-psidium-vc-insert))))
   `(diff-hl-delete ((,guava-psidium-class (:background ,guava-psidium-vc-delete :foreground ,guava-psidium-vc-delete))))

   ;; line-number
   `(line-number ((,guava-psidium-class (:foreground ,guava-psidium-brown :height 1.35))))
   `(line-number-current-line ((,guava-psidium-class (:foreground ,guava-psidium-black :background ,guava-psidium-light-pink :weight bold :height 1.35))))

   ;; hl-line
   `(hl-line ((,guava-psidium-class (:background ,guava-psidium-light-pink))))

   ;; region
   `(region ((,guava-psidium-class (:background ,guava-psidium-oceanic-green))))

   ;; mode-line
   `(mode-line ((,guava-psidium-class (:background ,guava-psidium-guava-green :foreground ,guava-psidium-black))))
   `(mode-line-inactive ((,guava-psidium-class (:background ,guava-psidium-light-brown :foreground ,guava-psidium-white))))
   `(guava-visible-bell ((,guava-psidium-class (:background ,guava-psidium-deep-green :foreground ,guava-psidium-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-psidium-class (:foreground ,guava-psidium-black))))

   ;;borders
   `(vertical-border ((,guava-psidium-class (:foreground ,guava-psidium-light-brown))))

   ;; header-line
   `(header-line ((,guava-psidium-class (:background ,guava-psidium-pink :foreground ,guava-psidium-white))))
   `(which-func ((,guava-psidium-class (:background ,guava-psidium-pink :foreground ,guava-psidium-white))))

   ;; tab-bar
   `(tab-bar ((,guava-psidium-class (:background ,guava-psidium-guava-green :foreground ,guava-psidium-white))))
   `(tab-bar-tab ((,guava-psidium-class (:background ,guava-psidium-pink :foreground ,guava-psidium-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-psidium-class (:background ,guava-psidium-guava-green :foreground ,guava-psidium-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-psidium-class (:background ,guava-psidium-guava-green :foreground ,guava-psidium-white))))
   `(tab-line-tab ((,guava-psidium-class (:background ,guava-psidium-guava-green :foreground ,guava-psidium-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-psidium-class (:background ,guava-psidium-pink :foreground ,guava-psidium-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-psidium-class (:background ,guava-psidium-guava-green :foreground ,guava-psidium-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-psidium-class (:foreground ,guava-psidium-deep-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-psidium-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-psidium-class (:foreground ,guava-psidium-green :weight medium))))
   `(font-lock-string-face ((,guava-psidium-class (:foreground ,guava-psidium-brown :weight bold))))
   `(font-lock-keyword-face ((,guava-psidium-class (:foreground ,guava-psidium-light-purple :weight medium))))
   `(font-lock-builtin-face ((,guava-psidium-class (:foreground ,guava-psidium-deep-blue :weight medium))))
   `(font-lock-warning-face ((,guava-psidium-class (:foreground ,guava-psidium-red :weight medium))))
   `(font-lock-type-face ((,guava-psidium-class (:foreground ,guava-psidium-deep-green :weight medium))))
   `(font-lock-constant-face ((,guava-psidium-class (:foreground ,guava-psidium-antarctic-blue :weight medium))))
   `(font-lock-function-name-face ((,guava-psidium-class (:foreground ,guava-psidium-blue :weight medium))))
   `(font-lock-bracket-face ((,guava-psidium-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-psidium-class (:foreground ,guava-psidium-deep-orange :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-psidium-class (:background ,guava-psidium-orange))))

   ;; buttons
   `(link ((,guava-psidium-class (:foreground ,guava-psidium-light-blue :underline t :weight bold))))
   `(link-visited ((,guava-psidium-class (:foreground ,guava-psidium-light-purple :underline t :weight bold))))
   `(button ((,guava-psidium-class (:foreground ,guava-psidium-blue :underline t :weight bold))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'guava-psidium)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-psidium-theme.el ends here
