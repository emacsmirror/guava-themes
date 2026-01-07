;;; guava-prunus-theme.el --- An Emacs theme inspired by guava and cherry colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 29, 2025
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
;; A theme inspired by guava and cherry colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-prunus "A theme inspired by guava and cherry colors.")

(let* (
      (guava-prunus-class '((class color) (min-colors 257)))
      ;;(guava-prunus-black             "#000000")
      (guava-prunus-white             "#FFFFFF")

      (guava-prunus-cream             "#DEA2BD");fffef5,EBDCF5
      (guava-prunus-light-brown       "#735944")
      (guava-prunus-brown             "#4A301B");583c25,
      (guava-prunus-dark-brown        "#1A0E05");E9E4F9,3F271D,2E1E03,281A04,1A0E05,1C0E06,1C0F07

      (guava-prunus-light-green       "#52BC63")
      (guava-prunus-oceanic-green     "#3AC3A2")

      (guava-prunus-deep-orange       "#C46935");a0522d
      (guava-prunus-red               "#88190C");cb001e,d2191e
      (guava-prunus-pink              "#CD2788");dc6199,cd4f88

      (guava-prunus-blue              "#4534E3");4534e3,120cdc
      (guava-prunus-deep-blue         "#655DB0")
      (guava-prunus-antarctic-blue    "#8D76CA")
      (guava-prunus-cyan              "#008B8B")

      (guava-prunus-deep-purple       "#740CBE");800080
      (guava-prunus-purple-red        "#8B2252")

      (guava-prunus-error             "#FF0000");FF0000,bc0000,890014
      (guava-prunus-warning           "#F68511");F68511,ffc333
      (guava-prunus-success           "#23D734");228B22,007900

      (guava-prunus-vc-change         guava-prunus-blue)
      (guava-prunus-vc-insert         guava-prunus-success)
      (guava-prunus-vc-delete         guava-prunus-error)
      )

  (custom-theme-set-faces
   'guava-prunus

   ;; default for guava-prunus
   `(default ((,guava-prunus-class (:foreground ,guava-prunus-cream :background ,guava-prunus-dark-brown))))

   ;; error, warning, success
   `(error ((,guava-prunus-class (:foreground ,guava-prunus-error :weight bold))))
   `(warning ((,guava-prunus-class (:foreground ,guava-prunus-warning :weight bold))))
   `(success ((,guava-prunus-class (:foreground ,guava-prunus-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-prunus-class (:background ,guava-prunus-deep-blue :foreground ,guava-prunus-white))))

   ;; fringe
   `(fringe ((,guava-prunus-class (:background ,guava-prunus-dark-brown :foreground ,guava-prunus-cream))))
   `(diff-hl-change ((,guava-prunus-class (:background ,guava-prunus-vc-change :foreground ,guava-prunus-vc-change))))
   `(diff-hl-insert ((,guava-prunus-class (:background ,guava-prunus-vc-insert :foreground ,guava-prunus-vc-insert))))
   `(diff-hl-delete ((,guava-prunus-class (:background ,guava-prunus-vc-delete :foreground ,guava-prunus-vc-delete))))

   ;; line-number
   `(line-number ((,guava-prunus-class (:foreground ,guava-prunus-purple-red :height 1.35))))
   `(line-number-current-line ((,guava-prunus-class (:foreground ,guava-prunus-cream :background ,guava-prunus-brown :weight bold :height 1.35))))

   ;; hl-line
   `(hl-line ((,guava-prunus-class (:background ,guava-prunus-brown))))

   ;; region
   `(region ((,guava-prunus-class (:background ,guava-prunus-light-brown))))

   ;; mode-line
   `(mode-line ((,guava-prunus-class (:background ,guava-prunus-red :foreground ,guava-prunus-white))))
   `(mode-line-inactive ((,guava-prunus-class (:background ,guava-prunus-deep-orange :foreground ,guava-prunus-white))))
   `(guava-visible-bell ((,guava-prunus-class (:background ,guava-prunus-antarctic-blue :foreground ,guava-prunus-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-prunus-class (:foreground ,guava-prunus-white))))

   ;; borders
   `(vertical-border ((,guava-prunus-class (:foreground ,guava-prunus-pink))))

   ;; header-line
   `(header-line ((,guava-prunus-class (:background ,guava-prunus-red :foreground ,guava-prunus-white))))
   `(which-func ((,guava-prunus-class (:background ,guava-prunus-red :foreground ,guava-prunus-white))))

   ;; tab-bar
   `(tab-bar ((,guava-prunus-class (:background ,guava-prunus-pink :foreground ,guava-prunus-white))))
   `(tab-bar-tab ((,guava-prunus-class (:background ,guava-prunus-red :foreground ,guava-prunus-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-prunus-class (:background ,guava-prunus-pink :foreground ,guava-prunus-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-prunus-class (:background ,guava-prunus-pink :foreground ,guava-prunus-white))))
   `(tab-line-tab ((,guava-prunus-class (:background ,guava-prunus-pink :foreground ,guava-prunus-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-prunus-class (:background ,guava-prunus-red :foreground ,guava-prunus-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-prunus-class (:background ,guava-prunus-pink :foreground ,guava-prunus-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-prunus-class (:foreground ,guava-prunus-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-prunus-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-prunus-class (:foreground ,guava-prunus-light-green :weight medium))))
   `(font-lock-string-face ((,guava-prunus-class (:foreground ,guava-prunus-purple-red :weight bold))))
   `(font-lock-keyword-face ((,guava-prunus-class (:foreground ,guava-prunus-deep-purple :weight medium))))
   `(font-lock-builtin-face ((,guava-prunus-class (:foreground ,guava-prunus-deep-blue :weight medium))))
   `(font-lock-warning-face ((,guava-prunus-class (:foreground ,guava-prunus-error :weight medium))))
   `(font-lock-type-face ((,guava-prunus-class (:foreground ,guava-prunus-antarctic-blue :weight medium))))
   `(font-lock-constant-face ((,guava-prunus-class (:foreground ,guava-prunus-cyan :weight medium))))
   `(font-lock-function-name-face ((,guava-prunus-class (:foreground ,guava-prunus-blue :weight medium))))
   `(font-lock-bracket-face ((,guava-prunus-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-prunus-class (:foreground ,guava-prunus-deep-orange :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-prunus-class (:background ,guava-prunus-deep-orange))))

   ;; buttons
   `(link ((,guava-prunus-class (:foreground ,guava-prunus-blue :underline t :weight bold))))
   `(link-visited ((,guava-prunus-class (:foreground ,guava-prunus-oceanic-green :underline t :weight bold))))
   `(button ((,guava-prunus-class (:foreground ,guava-prunus-blue :underline t :weight bold))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'guava-prunus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-prunus-theme.el ends here
