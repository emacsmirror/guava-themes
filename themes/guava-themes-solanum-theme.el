;;; guava-themes-solanum-theme.el --- A theme inspired by eggplant colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Feb 22, 2026
;; Version: 0.11.3
;; Keywords: themes, faces, color
;; URL: http://github.com/bormoge/guava-themes
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
;; A theme inspired by eggplant, potato, and tomato colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-solanum "A theme inspired by eggplant, potato, and tomato colors.")

(let* (
      (solanum-class '((class color) (min-colors 257)))
      (solanum-black             "#000000")
      (solanum-white             "#FFFFFF")

      (solanum-shadow            "#b3b3b3")

      (solanum-red-tomato        "#cd151f")
      (solanum-orange            "#e98c85")

      (solanum-yellow-potato     "#fde8b9")

      (solanum-light-green       "#61ff96")
      (solanum-green             "#0d6d4b")

      (solanum-light-blue        "#8ec4de")
      (solanum-blue              "#0d62b2")

      (solanum-light-purple      "#d4cbff");c4aeff,ccbcff
      (solanum-purple            "#9c69e8");9e7ae8,9c69e8
      (solanum-purple-red        "#64143d");6a143d
      (solanum-dark-purple       "#672b5f");41143d
      (solanum-purple-black      "#130d1a")

      (solanum-error             "#FF0000")
      (solanum-warning           "#f6d909");F68511
      (solanum-success           "#23a334");239834

      (solanum-vc-change         solanum-blue)
      (solanum-vc-insert         solanum-success)
      (solanum-vc-delete         solanum-error))

  (custom-theme-set-faces
   'guava-themes-solanum

   ;; default for solanum
   `(default ((,solanum-class (:foreground ,solanum-white :background ,solanum-purple-black))))

   ;; error, warning, success
   `(error ((,solanum-class (:foreground ,solanum-error :weight bold))))
   `(warning ((,solanum-class (:foreground ,solanum-warning :weight bold))))
   `(success ((,solanum-class (:foreground ,solanum-success :weight bold))))

   ;; cursor
   `(cursor ((,solanum-class (:background ,solanum-yellow-potato :foreground ,solanum-black))))

   ;; fringe
   `(fringe ((,solanum-class (:background ,solanum-purple-black :foreground ,solanum-purple-black))))
   `(diff-hl-change ((,solanum-class (:background ,solanum-vc-change :foreground ,solanum-vc-change))))
   `(diff-hl-insert ((,solanum-class (:background ,solanum-vc-insert :foreground ,solanum-vc-insert))))
   `(diff-hl-delete ((,solanum-class (:background ,solanum-vc-delete :foreground ,solanum-vc-delete))))

   ;; line-number
   `(line-number ((,solanum-class (:foreground ,solanum-white))))
   `(line-number-current-line ((,solanum-class (:foreground ,solanum-light-green :background ,solanum-purple-red :weight bold))))

   ;; highlight
   `(highlight ((,solanum-class (:background ,solanum-purple-red))))

   ;; shadow
   `(shadow ((,solanum-class (:foreground ,solanum-shadow))))

   ;; region
   `(region ((,solanum-class (:background ,solanum-dark-purple))))

   ;; mode-line
   `(mode-line ((,solanum-class (:background ,solanum-dark-purple :foreground ,solanum-white))))
   `(mode-line-inactive ((,solanum-class (:background ,solanum-purple-red :foreground ,solanum-white))))
   `(visible-bell ((,solanum-class (:background ,solanum-orange :foreground ,solanum-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,solanum-class (:foreground ,solanum-yellow-potato))))

   ;; borders
   `(vertical-border ((,solanum-class (:foreground ,solanum-purple-black))))

   ;; header-line
   `(header-line ((,solanum-class (:background ,solanum-dark-purple :foreground ,solanum-white))))
   `(which-func ((,solanum-class (:background ,solanum-dark-purple :foreground ,solanum-white))))

   ;; tab-bar
   `(tab-bar ((,solanum-class (:background ,solanum-purple-black :foreground ,solanum-white))))
   `(tab-bar-tab ((,solanum-class (:background ,solanum-dark-purple :foreground ,solanum-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,solanum-class (:background ,solanum-purple-black :foreground ,solanum-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,solanum-class (:background ,solanum-purple-black :foreground ,solanum-white))))
   `(tab-line-tab ((,solanum-class (:background ,solanum-purple-black :foreground ,solanum-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,solanum-class (:background ,solanum-dark-purple :foreground ,solanum-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,solanum-class (:background ,solanum-purple-black :foreground ,solanum-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,solanum-class (:foreground ,solanum-red-tomato :weight bold :height 0.9))))
   `(tab-line-tab-special ((,solanum-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,solanum-class (:foreground ,solanum-red-tomato :weight medium))))
   `(font-lock-string-face ((,solanum-class (:foreground ,solanum-yellow-potato :weight medium))))
   `(font-lock-keyword-face ((,solanum-class (:foreground ,solanum-green :weight medium))))
   `(font-lock-builtin-face ((,solanum-class (:foreground ,solanum-light-blue :weight medium))))
   `(font-lock-warning-face ((,solanum-class (:foreground ,solanum-error :weight bold))))
   `(font-lock-type-face ((,solanum-class (:foreground ,solanum-orange :weight medium))))
   `(font-lock-constant-face ((,solanum-class (:foreground ,solanum-light-purple :weight medium))))
   `(font-lock-function-name-face ((,solanum-class (:foreground ,solanum-purple :weight medium))))
   `(font-lock-bracket-face ((,solanum-class (:weight medium))))
   `(font-lock-variable-name-face ((,solanum-class (:foreground ,solanum-light-green :weight medium))))

   ;; parentheses
   `(show-paren-match ((,solanum-class (:foreground ,solanum-purple-black :background ,solanum-light-blue))))
   `(show-paren-mismatch ((,solanum-class (:foreground ,solanum-white :background ,solanum-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,solanum-class (:background ,solanum-error))))

   ;; buttons
   `(link ((,solanum-class (:foreground ,solanum-light-purple :underline t :weight bold))))
   `(link-visited ((,solanum-class (:foreground ,solanum-purple :underline t :weight bold))))
   `(button ((,solanum-class (:foreground ,solanum-light-purple :underline t :weight bold))))


   ;; external packages

   ;; doom-modeline
   `(doom-modeline-project-name ((,solanum-class (:foreground ,solanum-red-tomato))))
   `(doom-modeline-project-parent-dir ((,solanum-class (:foreground ,solanum-red-tomato))))
   `(doom-modeline-buffer-minor-mode ((,solanum-class (:foreground ,solanum-yellow-potato))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,solanum-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-solanum)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-solanum-theme.el ends here
