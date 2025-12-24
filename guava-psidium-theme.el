;;; guava-psidium-theme.el --- An Emacs theme purely inspired by guava colors. -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 07, 2025
;; Version: 0.1.0
;; Keywords: themes faces
;; URL: http://github.com/bormoge/guava-themes.el
;; Package-Requires: (emacs "30.2")

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

(let ((guava-psidium-class '((class color) (min-colors 257)))
      (guava-psidium-black         "#000000")

      (guava-psidium-white         "#FFFFFF")
      (guava-psidium-cream         "#FAEECE");F7DEB6
      (guava-psidium-yellow        "#FFE582")

      (guava-psidium-light-green   "#d0f4bc");B9FFA8
      (guava-psidium-green         "#048704");058c05,047804,046a04
      (guava-psidium-alt-green     "#599f48")
      (guava-psidium-chartreuse    "#7F8263")
      (guava-psidium-guava-green   "#aeca41")

      (guava-psidium-red           "#FF7D5F")
      (guava-psidium-orange        "#F87C4A")
      (guava-psidium-light-pink    "#fcd0c9");F8917C
      (guava-psidium-pink          "#F8767C");F84865,F8767C,F88686,F85A65

      (guava-psidium-light-brown   "#735944")
      (guava-psidium-brown         "#55422a")

      (guava-psidium-light-blue    "#41c3ca")
      (guava-psidium-blue          "#008b8b")
      (guava-psidium-deep-blue     "#004F5D"))

  (custom-theme-set-faces
   'guava-psidium

   ;; default for guava-psidium
   `(default ((,guava-psidium-class (:foreground ,guava-psidium-black :background ,guava-psidium-cream))))

   ;; cursor
   `(cursor ((,guava-psidium-class (:background ,guava-psidium-alt-green :foreground ,guava-psidium-white))))

   ;; fridge
   `(fringe ((,guava-psidium-class (:background ,guava-psidium-cream :foreground ,guava-psidium-cream))))
   `(diff-hl-change ((,guava-psidium-class (:background ,guava-psidium-light-blue :foreground ,guava-psidium-light-blue))))
   `(diff-hl-insert ((,guava-psidium-class (:background ,guava-psidium-green :foreground ,guava-psidium-green))))
   `(diff-hl-delete ((,guava-psidium-class (:background ,guava-psidium-red :foreground ,guava-psidium-blue))))

   ;; ;; line-number
   `(line-number ((,guava-psidium-class (:foreground ,guava-psidium-brown :height 1.35))))
   `(line-number-current-line ((,guava-psidium-class (:foreground ,guava-psidium-black :background ,guava-psidium-light-pink :weight bold :height 1.35))))

   ;; background
   `(region ((,guava-psidium-class (:background ,guava-psidium-green :foreground ,guava-psidium-black))))
   `(hl-line ((,guava-psidium-class (:background ,guava-psidium-light-pink))))

   ;; mode-line
   `(mode-line ((,guava-psidium-class (:background ,guava-psidium-guava-green :foreground ,guava-psidium-black :box (:line-width 2 :color ,guava-psidium-guava-green)))))
   `(mode-line-inactive ((,guava-psidium-class (:background ,guava-psidium-light-brown :foreground ,guava-psidium-white :box (:line-width 2 :color ,guava-psidium-light-brown)))))
   `(guava-visible-bell ((,guava-psidium-class (:background ,guava-psidium-light-brown :foreground ,guava-psidium-white))))

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
   `(tab-bar-tab-inactive ((,guava-psidium-class (:background ,guava-psidium-guava-green :foreground ,guava-psidium-white :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-psidium-class (:background ,guava-psidium-guava-green :foreground ,guava-psidium-white))))
   `(tab-line-tab ((,guava-psidium-class (:background ,guava-psidium-guava-green :foreground ,guava-psidium-white :height 0.9))))
   `(tab-line-tab-current ((,guava-psidium-class (:background ,guava-psidium-pink :foreground ,guava-psidium-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-psidium-class (:background ,guava-psidium-guava-green :foreground ,guava-psidium-white :height 0.9))))
   `(tab-line-tab-modified ((,guava-psidium-class (:foreground ,guava-psidium-deep-blue :height 0.9))))
   `(tab-line-tab-special ((,guava-psidium-class (:slant italic :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-psidium-class (:foreground ,guava-psidium-green))))
   ;; `(font-lock-bracket-face ((,guava-psidium-class (:foreground ,guava-psidium-black :background ,guava-psidium-light-grey))))
   ;; `(font-lock-function-name-face ((,guava-psidium-class (:foreground ,guava-psidium-black :background ,guava-psidium-light-grey))))
   ;; `(font-lock-builtin-face ((,guava-psidium-class (:foreground ,guava-psidium-blue))))
   ;; `(font-lock-constant-face ((,guava-psidium-class (:foreground ,guava-psidium-br-magenta))))
   ;; `(font-lock-keyword-face ((,guava-psidium-class (:foreground ,guava-psidium-blue))))
   ;; `(font-lock-string-face ((,guava-psidium-class (:foreground ,guava-psidium-red))))
   ;; `(font-lock-type-face ((,guava-psidium-class (:foreground ,guava-psidium-brown))))
   ;; `(font-lock-variable-name-face ((,guava-psidium-class (:foreground ,guava-psidium-black))))
   ;; `(font-lock-warning-face ((,guava-psidium-class (:foreground ,guava-psidium-br-red :background ,guava-psidium-yellow :weight bold))))

   ;; parentheses
   `(show-paren-match ((,guava-psidium-class (:background ,guava-psidium-red))))

   ;; ;; buttons
   ;; `(link ((,guava-psidium-class (:foreground ,guava-psidium-red :underline t :weight bold))))
   ;; `(link-visited ((,guava-psidium-class (:foreground ,guava-psidium-brown :underline t))))
   ;; `(button ((,guava-psidium-class (:background ,guava-psidium-light-grey :foreground ,guava-psidium-black :box (:style released-button)))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'guava-psidium)
;;; guava-psidium-theme.el ends here
