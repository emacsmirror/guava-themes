;;; guava-dracaena-theme.el --- An Emacs theme inspired by guava and dragon tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 06, 2026
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
;; A theme inspired by guava and dragon tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-dracaena "A theme inspired by guava and dragon tree colors.")

(let* (
      (guava-dracaena-class '((class color) (min-colors 257)))
      (guava-dracaena-black             "#000000")
      (guava-dracaena-white             "#FFFFFF")

      (guava-dracaena-light-gray        "#4f4f4f")
      (guava-dracaena-gray              "#424845");404242,3e4040,3d3f3f,424545
      (guava-dracaena-dark-gray         "#353838")

      (guava-dracaena-guava-green       "#AED734")
      (guava-dracaena-deep-green        "#298e25")

      (guava-dracaena-snakeplant-yellow "#d9d389")

      (guava-dracaena-light-orange      "#ffa07a")
      (guava-dracaena-red               "#d32333");c91628,cc192a,d7192a,ed2725,e92333,db2333
      (guava-dracaena-deep-red          "#792725")

      (guava-dracaena-light-brown       "#8b6c4d")

      (guava-dracaena-light-blue        "#425fd5")
      (guava-dracaena-blue              "#2134d5")
      (guava-dracaena-antarctic-blue    "#bacce4");4f62be
      (guava-dracaena-steel-blue        "#4f94cd");b0c4de

      (guava-dracaena-pink-purple       "#a5225c");961250,b7125c
      (guava-dracaena-deep-purple       "#8f1ac8")

      (guava-dracaena-error             "#FF0000")
      (guava-dracaena-warning           "#F68511")
      (guava-dracaena-success           "#29d925")

      (guava-dracaena-vc-change         guava-dracaena-blue)
      (guava-dracaena-vc-insert         guava-dracaena-deep-green)
      (guava-dracaena-vc-delete         guava-dracaena-error)
      )

  (custom-theme-set-faces
   'guava-dracaena

   ;; default for guava-dracaena
   `(default ((,guava-dracaena-class (:foreground ,guava-dracaena-antarctic-blue :background ,guava-dracaena-gray))))

   ;; error, warning, success
   `(error ((,guava-dracaena-class (:foreground ,guava-dracaena-error :weight bold))))
   `(warning ((,guava-dracaena-class (:foreground ,guava-dracaena-warning :weight bold))))
   `(success ((,guava-dracaena-class (:foreground ,guava-dracaena-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-dracaena-class (:background ,guava-dracaena-light-orange :foreground ,guava-dracaena-white))))

   ;; fringe
   `(fringe ((,guava-dracaena-class (:background ,guava-dracaena-gray :foreground ,guava-dracaena-gray))))
   `(diff-hl-change ((,guava-dracaena-class (:background ,guava-dracaena-vc-change :foreground ,guava-dracaena-vc-change))))
   `(diff-hl-insert ((,guava-dracaena-class (:background ,guava-dracaena-vc-insert :foreground ,guava-dracaena-vc-insert))))
   `(diff-hl-delete ((,guava-dracaena-class (:background ,guava-dracaena-vc-delete :foreground ,guava-dracaena-vc-delete))))

   ;; line-number
   `(line-number ((,guava-dracaena-class (:foreground ,guava-dracaena-antarctic-blue :height 1.35))))
   `(line-number-current-line ((,guava-dracaena-class (:foreground ,guava-dracaena-light-orange :background ,guava-dracaena-light-gray :weight bold :height 1.35))))

   ;; hl-line
   `(hl-line ((,guava-dracaena-class (:background ,guava-dracaena-light-gray))))

   ;; region
   `(region ((,guava-dracaena-class (:background ,guava-dracaena-deep-red))))

   ;; mode-line
   `(mode-line ((,guava-dracaena-class (:background ,guava-dracaena-deep-red :foreground ,guava-dracaena-white))))
   `(mode-line-inactive ((,guava-dracaena-class (:background ,guava-dracaena-dark-gray :foreground ,guava-dracaena-white))))
   `(guava-visible-bell ((,guava-dracaena-class (:background ,guava-dracaena-light-orange :foreground ,guava-dracaena-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-dracaena-class (:foreground ,guava-dracaena-antarctic-blue))))

   ;; borders
   `(vertical-border ((,guava-dracaena-class (:foreground ,guava-dracaena-light-orange))))

   ;; header-line
   `(header-line ((,guava-dracaena-class (:background ,guava-dracaena-deep-red :foreground ,guava-dracaena-white))))
   `(which-func ((,guava-dracaena-class (:background ,guava-dracaena-deep-red :foreground ,guava-dracaena-white))))

   ;; tab-bar
   `(tab-bar ((,guava-dracaena-class (:background ,guava-dracaena-dark-gray :foreground ,guava-dracaena-white))))
   `(tab-bar-tab ((,guava-dracaena-class (:background ,guava-dracaena-deep-red :foreground ,guava-dracaena-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-dracaena-class (:background ,guava-dracaena-dark-gray :foreground ,guava-dracaena-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-dracaena-class (:background ,guava-dracaena-dark-gray :foreground ,guava-dracaena-white))))
   `(tab-line-tab ((,guava-dracaena-class (:background ,guava-dracaena-dark-gray :foreground ,guava-dracaena-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-dracaena-class (:background ,guava-dracaena-deep-red :foreground ,guava-dracaena-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-dracaena-class (:background ,guava-dracaena-dark-gray :foreground ,guava-dracaena-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-dracaena-class (:foreground ,guava-dracaena-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-dracaena-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-dracaena-class (:foreground ,guava-dracaena-deep-green :weight medium))))
   `(font-lock-string-face ((,guava-dracaena-class (:foreground ,guava-dracaena-snakeplant-yellow :weight bold))))
   `(font-lock-keyword-face ((,guava-dracaena-class (:foreground ,guava-dracaena-pink-purple :weight medium))))
   `(font-lock-builtin-face ((,guava-dracaena-class (:foreground ,guava-dracaena-red :weight medium))))
   `(font-lock-warning-face ((,guava-dracaena-class (:foreground ,guava-dracaena-error :weight medium))))
   `(font-lock-type-face ((,guava-dracaena-class (:foreground ,guava-dracaena-guava-green :weight medium))))
   `(font-lock-constant-face ((,guava-dracaena-class (:foreground ,guava-dracaena-light-blue :weight medium))))
   `(font-lock-function-name-face ((,guava-dracaena-class (:foreground ,guava-dracaena-light-orange :weight medium))))
   `(font-lock-bracket-face ((,guava-dracaena-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-dracaena-class (:foreground ,guava-dracaena-deep-purple :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-dracaena-class (:background ,guava-dracaena-steel-blue))))

   ;; buttons
   `(link ((,guava-dracaena-class (:foreground ,guava-dracaena-red :underline t :weight bold))))
   `(link-visited ((,guava-dracaena-class (:foreground ,guava-dracaena-light-orange :underline t :weight bold))))
   `(button ((,guava-dracaena-class (:foreground ,guava-dracaena-red :underline t :weight bold))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'guava-dracaena)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-dracaena-theme.el ends here
