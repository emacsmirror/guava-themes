;;; guava-jacaranda-theme.el --- An Emacs theme inspired by guava and jacaranda colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 27, 2025
;; Version: 0.4.1
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
;; A theme inspired by guava and jacaranda colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-jacaranda "A theme inspired by guava and jacaranda colors.")

(let* (
      (guava-jacaranda-class '((class color) (min-colors 257)))
      (guava-jacaranda-black             "#000000")

      (guava-jacaranda-white             "#FFFFFF")
      (guava-jacaranda-cream             "#e9d9f9");e9e4f9

      (guava-jacaranda-light-green       "#52bc63")
      (guava-jacaranda-green             "#8ec654")
      (guava-jacaranda-deep-green        "#267a63")
      (guava-jacaranda-oceanic-green     "#3ab992")

      (guava-jacaranda-orange            "#ff9f79");ff9535
      (guava-jacaranda-deep-orange       "#c46935");a0522d
      (guava-jacaranda-red               "#ca0036")

      (guava-jacaranda-light-blue        "#C0B4E4")
      (guava-jacaranda-blue              "#4534e3")
      (guava-jacaranda-deep-blue         "#655db0")
      (guava-jacaranda-antarctic-blue    "#8d76ca")
      (guava-jacaranda-cyan              "#008b8b")

      (guava-jacaranda-light-purple      "#dbd0fd")
      (guava-jacaranda-purple            "#aa69e6");984ee6
      (guava-jacaranda-deep-purple       "#740cbe");800080
      (guava-jacaranda-purple-red        "#8b2252")

      (guava-jacaranda-error             "#bc0000");FF0000
      (guava-jacaranda-warning           "#ffc333");F68511
      (guava-jacaranda-success           "#007900");228B22

      (guava-jacaranda-vc-change         guava-jacaranda-blue)
      (guava-jacaranda-vc-insert         guava-jacaranda-green)
      (guava-jacaranda-vc-delete         guava-jacaranda-red)
      )

  (custom-theme-set-faces
   'guava-jacaranda

   ;; default for guava-jacaranda
   `(default ((,guava-jacaranda-class (:foreground ,guava-jacaranda-black :background ,guava-jacaranda-cream))))

   ;; error, warning, success
   `(error ((,guava-jacaranda-class (:foreground ,guava-jacaranda-error :weight bold))))
   `(warning ((,guava-jacaranda-class (:foreground ,guava-jacaranda-warning :weight bold))))
   `(success ((,guava-jacaranda-class (:foreground ,guava-jacaranda-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-jacaranda-class (:background ,guava-jacaranda-purple :foreground ,guava-jacaranda-white))))

   ;; fringe
   `(fringe ((,guava-jacaranda-class (:background ,guava-jacaranda-cream :foreground ,guava-jacaranda-cream))))
   `(diff-hl-change ((,guava-jacaranda-class (:background ,guava-jacaranda-vc-change :foreground ,guava-jacaranda-vc-change))))
   `(diff-hl-insert ((,guava-jacaranda-class (:background ,guava-jacaranda-vc-insert :foreground ,guava-jacaranda-vc-insert))))
   `(diff-hl-delete ((,guava-jacaranda-class (:background ,guava-jacaranda-vc-delete :foreground ,guava-jacaranda-vc-delete))))

   ;; line-number
   `(line-number ((,guava-jacaranda-class (:foreground ,guava-jacaranda-antarctic-blue :height 1.35))))
   `(line-number-current-line ((,guava-jacaranda-class (:foreground ,guava-jacaranda-black :background ,guava-jacaranda-light-purple :weight bold :height 1.35))))

   ;; hl-line
   `(hl-line ((,guava-jacaranda-class (:background ,guava-jacaranda-light-purple))))

   ;; region
   `(region ((,guava-jacaranda-class (:background ,guava-jacaranda-light-blue))))

   ;; mode-line
   `(mode-line ((,guava-jacaranda-class (:background ,guava-jacaranda-purple :foreground ,guava-jacaranda-white))))
   `(mode-line-inactive ((,guava-jacaranda-class (:background ,guava-jacaranda-light-blue :foreground ,guava-jacaranda-white))))
   `(guava-visible-bell ((,guava-jacaranda-class (:background ,guava-jacaranda-orange :foreground ,guava-jacaranda-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-jacaranda-class (:foreground ,guava-jacaranda-black))))

   ;; borders
   `(vertical-border ((,guava-jacaranda-class (:foreground ,guava-jacaranda-light-purple))))

   ;; header-line
   `(header-line ((,guava-jacaranda-class (:background ,guava-jacaranda-deep-blue :foreground ,guava-jacaranda-white))))
   `(which-func ((,guava-jacaranda-class (:background ,guava-jacaranda-deep-blue :foreground ,guava-jacaranda-white))))

   ;; tab-bar
   `(tab-bar ((,guava-jacaranda-class (:background ,guava-jacaranda-purple :foreground ,guava-jacaranda-white))))
   `(tab-bar-tab ((,guava-jacaranda-class (:background ,guava-jacaranda-deep-blue :foreground ,guava-jacaranda-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-jacaranda-class (:background ,guava-jacaranda-purple :foreground ,guava-jacaranda-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-jacaranda-class (:background ,guava-jacaranda-purple :foreground ,guava-jacaranda-white))))
   `(tab-line-tab ((,guava-jacaranda-class (:background ,guava-jacaranda-purple :foreground ,guava-jacaranda-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-jacaranda-class (:background ,guava-jacaranda-deep-blue :foreground ,guava-jacaranda-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-jacaranda-class (:background ,guava-jacaranda-purple :foreground ,guava-jacaranda-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-jacaranda-class (:foreground ,guava-jacaranda-orange :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-jacaranda-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-jacaranda-class (:foreground ,guava-jacaranda-light-green :weight medium))))
   `(font-lock-string-face ((,guava-jacaranda-class (:foreground ,guava-jacaranda-purple-red :weight bold))))
   `(font-lock-keyword-face ((,guava-jacaranda-class (:foreground ,guava-jacaranda-deep-purple :weight medium))))
   `(font-lock-builtin-face ((,guava-jacaranda-class (:foreground ,guava-jacaranda-deep-blue :weight medium))))
   `(font-lock-warning-face ((,guava-jacaranda-class (:foreground ,guava-jacaranda-red :weight medium))))
   `(font-lock-type-face ((,guava-jacaranda-class (:foreground ,guava-jacaranda-oceanic-green :weight medium))))
   `(font-lock-constant-face ((,guava-jacaranda-class (:foreground ,guava-jacaranda-cyan :weight medium))))
   `(font-lock-function-name-face ((,guava-jacaranda-class (:foreground ,guava-jacaranda-blue :weight medium))))
   `(font-lock-bracket-face ((,guava-jacaranda-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-jacaranda-class (:foreground ,guava-jacaranda-deep-orange :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-jacaranda-class (:background ,guava-jacaranda-orange))))

   ;; buttons
   `(link ((,guava-jacaranda-class (:foreground ,guava-jacaranda-oceanic-green :underline t :weight bold))))
   `(link-visited ((,guava-jacaranda-class (:foreground ,guava-jacaranda-deep-green :underline t :weight bold))))
   `(button ((,guava-jacaranda-class (:foreground ,guava-jacaranda-blue :underline t :weight bold))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'guava-jacaranda)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-jacaranda-theme.el ends here
