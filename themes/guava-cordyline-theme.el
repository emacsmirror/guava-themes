;;; guava-cordyline-theme.el --- A theme inspired by the ti plant colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 12, 2026
;; Version: 0.6.1
;; Keywords: themes, faces, color
;; URL: http://github.com/bormoge/guava-themes.el
;; Package-Requires: ((emacs "24.1"))
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
;; A theme inspired by the ti plant colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-cordyline "A theme inspired by the ti plant colors.")

(let* (
      (guava-cordyline-class '((class color) (min-colors 257)))
      (guava-cordyline-black             "#000000")
      (guava-cordyline-white             "#FFFFFF")

      (guava-cordyline-pink              "#CB5F68")
      (guava-cordyline-pink-purple       "#da70d6")

      (guava-cordyline-light-blue        "#5f70cb")
      (guava-cordyline-blue              "#2134d5")
      (guava-cordyline-deep-blue         "#1330af")
      (guava-cordyline-steel-blue        "#4f94cd")
      (guava-cordyline-dark-cyan         "#00708b")

      (guava-cordyline-light-purple      "#a21cd1");8b1cb0,9c1cbc,991cbc
      (guava-cordyline-purple            "#703aaf");54366d,583675,583683,583d83,59338f,673b94
      (guava-cordyline-deep-purple       "#392b38");3f323c
      (guava-cordyline-alt-purple        "#473b4c");433640,43364c
      (guava-cordyline-purple-red        "#8d2a46")
      (guava-cordyline-dark-purple       "#211730")

      (guava-cordyline-error             "#FF0000")
      (guava-cordyline-warning           "#f6d909");F68511
      (guava-cordyline-success           "#23a334");239834

      (guava-cordyline-vc-change         guava-cordyline-blue)
      (guava-cordyline-vc-insert         guava-cordyline-success)
      (guava-cordyline-vc-delete         guava-cordyline-error))

  (custom-theme-set-faces
   'guava-cordyline

   ;; default for guava-cordyline
   `(default ((,guava-cordyline-class (:foreground ,guava-cordyline-white :background ,guava-cordyline-deep-purple))))

   ;; error, warning, success
   `(error ((,guava-cordyline-class (:foreground ,guava-cordyline-error :weight bold))))
   `(warning ((,guava-cordyline-class (:foreground ,guava-cordyline-warning :weight bold))))
   `(success ((,guava-cordyline-class (:foreground ,guava-cordyline-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-cordyline-class (:background ,guava-cordyline-pink :foreground ,guava-cordyline-white))))

   ;; fringe
   `(fringe ((,guava-cordyline-class (:background ,guava-cordyline-deep-purple :foreground ,guava-cordyline-deep-purple))))
   `(diff-hl-change ((,guava-cordyline-class (:background ,guava-cordyline-vc-change :foreground ,guava-cordyline-vc-change))))
   `(diff-hl-insert ((,guava-cordyline-class (:background ,guava-cordyline-vc-insert :foreground ,guava-cordyline-vc-insert))))
   `(diff-hl-delete ((,guava-cordyline-class (:background ,guava-cordyline-vc-delete :foreground ,guava-cordyline-vc-delete))))

   ;; line-number
   `(line-number ((,guava-cordyline-class (:foreground ,guava-cordyline-white :height 1.35))))
   `(line-number-current-line ((,guava-cordyline-class (:foreground ,guava-cordyline-light-blue :background ,guava-cordyline-alt-purple :weight bold :height 1.35))))

   ;; hl-line
   `(hl-line ((,guava-cordyline-class (:background ,guava-cordyline-alt-purple))))

   ;; region
   `(region ((,guava-cordyline-class (:background ,guava-cordyline-dark-purple))))

   ;; mode-line
   `(mode-line ((,guava-cordyline-class (:background ,guava-cordyline-purple-red :foreground ,guava-cordyline-white))))
   `(mode-line-inactive ((,guava-cordyline-class (:background ,guava-cordyline-dark-purple :foreground ,guava-cordyline-white))))
   `(guava-themes-visible-bell ((,guava-cordyline-class (:background ,guava-cordyline-light-blue :foreground ,guava-cordyline-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-cordyline-class (:foreground ,guava-cordyline-white))))

   ;; borders
   `(vertical-border ((,guava-cordyline-class (:foreground ,guava-cordyline-deep-purple))))

   ;; header-line
   `(header-line ((,guava-cordyline-class (:background ,guava-cordyline-purple-red :foreground ,guava-cordyline-white))))
   `(which-func ((,guava-cordyline-class (:background ,guava-cordyline-purple-red :foreground ,guava-cordyline-white))))

   ;; tab-bar
   `(tab-bar ((,guava-cordyline-class (:background ,guava-cordyline-deep-purple :foreground ,guava-cordyline-white))))
   `(tab-bar-tab ((,guava-cordyline-class (:background ,guava-cordyline-purple-red :foreground ,guava-cordyline-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-cordyline-class (:background ,guava-cordyline-deep-purple :foreground ,guava-cordyline-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-cordyline-class (:background ,guava-cordyline-deep-purple :foreground ,guava-cordyline-white))))
   `(tab-line-tab ((,guava-cordyline-class (:background ,guava-cordyline-deep-purple :foreground ,guava-cordyline-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-cordyline-class (:background ,guava-cordyline-purple-red :foreground ,guava-cordyline-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-cordyline-class (:background ,guava-cordyline-deep-purple :foreground ,guava-cordyline-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-cordyline-class (:foreground ,guava-cordyline-light-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-cordyline-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-cordyline-class (:foreground ,guava-cordyline-light-blue :weight medium))))
   `(font-lock-string-face ((,guava-cordyline-class (:foreground ,guava-cordyline-pink :weight bold))))
   `(font-lock-keyword-face ((,guava-cordyline-class (:foreground ,guava-cordyline-purple :weight medium))))
   `(font-lock-builtin-face ((,guava-cordyline-class (:foreground ,guava-cordyline-purple-red :weight medium))))
   `(font-lock-warning-face ((,guava-cordyline-class (:foreground ,guava-cordyline-error :weight medium))))
   `(font-lock-type-face ((,guava-cordyline-class (:foreground ,guava-cordyline-light-purple :weight medium))))
   `(font-lock-constant-face ((,guava-cordyline-class (:foreground ,guava-cordyline-dark-cyan :weight medium))))
   `(font-lock-function-name-face ((,guava-cordyline-class (:foreground ,guava-cordyline-pink-purple :weight medium))))
   `(font-lock-bracket-face ((,guava-cordyline-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-cordyline-class (:foreground ,guava-cordyline-deep-blue :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-cordyline-class (:background ,guava-cordyline-steel-blue))))

   ;; buttons
   `(link ((,guava-cordyline-class (:foreground ,guava-cordyline-steel-blue :underline t :weight bold))))
   `(link-visited ((,guava-cordyline-class (:foreground ,guava-cordyline-purple :underline t :weight bold))))
   `(button ((,guava-cordyline-class (:foreground ,guava-cordyline-steel-blue :underline t :weight bold))))))

(provide-theme 'guava-cordyline)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-cordyline-theme.el ends here
