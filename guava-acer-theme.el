;;; guava-acer-theme.el --- An Emacs theme inspired by guava and maple colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 12, 2026
;; Version: 0.5.0
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
;; A theme inspired by guava and maple colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-acer "A theme inspired by guava and maple colors.")

(let* (
      (guava-acer-class '((class color) (min-colors 257)))
      (guava-acer-black             "#000000")
      (guava-acer-white             "#FFFFFF")

      (guava-acer-cream             "#dc8e64"); f68e64

      (guava-acer-deep-green        "#239834")
      (guava-acer-maple             "#54c924");a6c924

      (guava-acer-orange            "#cb470a");e96d37,d3470a
      (guava-acer-alt-orange        "#f26b3b")
      (guava-acer-deep-orange       "#d85820")
      (guava-acer-red               "#a9251e");cf0501
      (guava-acer-pink              "#ec31a3")
      (guava-acer-autumn            "#f68b47");c14c5c,f46157,db3d32,e14337,ed7038,f77b44,f3814f,e8674a,ed674a

      (guava-acer-blue              "#2134d5")
      (guava-acer-oceanic-blue      "#3a9187");bacce4

      (guava-acer-light-purple      "#7927ac");6e279e
      (guava-acer-purple            "#9e4d76")
      (guava-acer-deep-purple       "#663c6c")
      (guava-acer-purple-red        "#9f234b")

      (guava-acer-error             "#FF0000")
      (guava-acer-warning           "#F68511")
      (guava-acer-success           "#239834");23D734

      (guava-acer-vc-change         guava-acer-blue)
      (guava-acer-vc-insert         guava-acer-success)
      (guava-acer-vc-delete         guava-acer-error)
      )

  (custom-theme-set-faces
   'guava-acer

   ;; default for guava-acer
   `(default ((,guava-acer-class (:foreground ,guava-acer-black :background ,guava-acer-autumn))))

   ;; error, warning, success
   `(error ((,guava-acer-class (:foreground ,guava-acer-error :weight bold))))
   `(warning ((,guava-acer-class (:foreground ,guava-acer-warning :weight bold))))
   `(success ((,guava-acer-class (:foreground ,guava-acer-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-acer-class (:background ,guava-acer-deep-green :foreground ,guava-acer-black))))

   ;; fringe
   `(fringe ((,guava-acer-class (:background ,guava-acer-autumn :foreground ,guava-acer-black))))
   `(diff-hl-change ((,guava-acer-class (:background ,guava-acer-vc-change :foreground ,guava-acer-vc-change))))
   `(diff-hl-insert ((,guava-acer-class (:background ,guava-acer-vc-insert :foreground ,guava-acer-vc-insert))))
   `(diff-hl-delete ((,guava-acer-class (:background ,guava-acer-vc-delete :foreground ,guava-acer-vc-delete))))

   ;; line-number
   `(line-number ((,guava-acer-class (:foreground ,guava-acer-black :height 1.35))))
   `(line-number-current-line ((,guava-acer-class (:foreground ,guava-acer-red :background ,guava-acer-cream :weight bold :height 1.35))))

   ;; hl-line
   `(hl-line ((,guava-acer-class (:background ,guava-acer-cream))))

   ;; region
   `(region ((,guava-acer-class (:background ,guava-acer-deep-orange))))

   ;; mode-line
   `(mode-line ((,guava-acer-class (:background ,guava-acer-purple-red :foreground ,guava-acer-white))))
   `(mode-line-inactive ((,guava-acer-class (:background ,guava-acer-deep-purple :foreground ,guava-acer-white))))
   `(guava-visible-bell ((,guava-acer-class (:background ,guava-acer-maple :foreground ,guava-acer-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-acer-class (:foreground ,guava-acer-black))))

   ;; borders
   `(vertical-border ((,guava-acer-class (:foreground ,guava-acer-autumn))))

   ;; header-line
   `(header-line ((,guava-acer-class (:background ,guava-acer-purple-red :foreground ,guava-acer-white))))
   `(which-func ((,guava-acer-class (:background ,guava-acer-purple-red :foreground ,guava-acer-white))))

   ;; tab-bar
   `(tab-bar ((,guava-acer-class (:background ,guava-acer-purple :foreground ,guava-acer-white))))
   `(tab-bar-tab ((,guava-acer-class (:background ,guava-acer-purple-red :foreground ,guava-acer-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-acer-class (:background ,guava-acer-purple :foreground ,guava-acer-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-acer-class (:background ,guava-acer-purple :foreground ,guava-acer-white))))
   `(tab-line-tab ((,guava-acer-class (:background ,guava-acer-purple :foreground ,guava-acer-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-acer-class (:background ,guava-acer-purple-red :foreground ,guava-acer-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-acer-class (:background ,guava-acer-purple :foreground ,guava-acer-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-acer-class (:foreground ,guava-acer-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-acer-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-acer-class (:foreground ,guava-acer-red :weight medium))))
   `(font-lock-string-face ((,guava-acer-class (:foreground ,guava-acer-orange :weight bold))))
   `(font-lock-keyword-face ((,guava-acer-class (:foreground ,guava-acer-purple-red :weight medium))))
   `(font-lock-builtin-face ((,guava-acer-class (:foreground ,guava-acer-purple :weight medium))))
   `(font-lock-warning-face ((,guava-acer-class (:foreground ,guava-acer-error :weight medium))))
   `(font-lock-type-face ((,guava-acer-class (:foreground ,guava-acer-deep-purple :weight medium))))
   `(font-lock-constant-face ((,guava-acer-class (:foreground ,guava-acer-deep-green :weight medium))))
   `(font-lock-function-name-face ((,guava-acer-class (:foreground ,guava-acer-oceanic-blue :weight medium))))
   `(font-lock-bracket-face ((,guava-acer-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-acer-class (:foreground ,guava-acer-pink :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-acer-class (:background ,guava-acer-purple))))

   ;; buttons
   `(link ((,guava-acer-class (:foreground ,guava-acer-oceanic-blue :underline t :weight bold))))
   `(link-visited ((,guava-acer-class (:foreground ,guava-acer-light-purple :underline t :weight bold))))
   `(button ((,guava-acer-class (:foreground ,guava-acer-oceanic-blue :underline t :weight bold))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'guava-acer)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-acer-theme.el ends here
