;;; guava-themes-ceiba-theme.el --- A theme inspired by the ceiba tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 21, 2026
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
;; A theme inspired by the ceiba tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-ceiba "A theme inspired by the ceiba tree colors.")

(let* (
      (ceiba-class '((class color) (min-colors 257)))
      (ceiba-black             "#000000")
      (ceiba-white             "#FFFFFF")

      (ceiba-shadow            "#7f7f7f")

      ;; (ceiba-light-gray        "#bab49e");dcdcdc,d4d4d4
      (ceiba-gray-green        "#bab49e");8c857b,8d8d8b,8c7f76,817a68,817a6a
      (ceiba-gray              "#9d9d9d");dcdcdc,656865,7f7f89,787882
      (ceiba-gray-blue         "#798585");8c857b,8d8d8b,6d726a,656865

      (ceiba-green             "#5b6452");5a6352
      (ceiba-deep-green        "#2b5535");375033,395235,3a5435,3a5835,385635,375535
      (ceiba-green-blue        "#116452");5a6352

      (ceiba-orange            "#a85639")

      (ceiba-steel-blue        "#aabed8");b0c4de
      (ceiba-blue              "#2327dc")
      (ceiba-alt-blue          "#2268a7");3a5ba7

      (ceiba-purple            "#4e466d");49206d
      (ceiba-purple-red        "#762362");862060,892362

      (ceiba-brown-sand        "#826e51");796041
      (ceiba-light-brown       "#6d4b30");6b492e
      (ceiba-brown-wood        "#53453d");9c6d85,bf8987,514141,53423e

      (ceiba-error             "#ff0000");ff0000,d70000
      (ceiba-warning           "#f6c911");F68511
      (ceiba-success           "#29c825")

      (ceiba-vc-change         ceiba-blue)
      (ceiba-vc-insert         ceiba-success)
      (ceiba-vc-delete         ceiba-error))

  (custom-theme-set-faces
   'guava-themes-ceiba

   ;; default for ceiba
   `(default ((,ceiba-class (:foreground ,ceiba-black :background ,ceiba-gray-green))))

   ;; error, warning, success
   `(error ((,ceiba-class (:foreground ,ceiba-error :weight bold))))
   `(warning ((,ceiba-class (:foreground ,ceiba-warning :weight bold))))
   `(success ((,ceiba-class (:foreground ,ceiba-success :weight bold))))

   ;; cursor
   `(cursor ((,ceiba-class (:background ,ceiba-brown-wood :foreground ,ceiba-black))))

   ;; fringe
   `(fringe ((,ceiba-class (:background ,ceiba-gray-green :foreground ,ceiba-black))))
   `(diff-hl-change ((,ceiba-class (:background ,ceiba-vc-change :foreground ,ceiba-vc-change))))
   `(diff-hl-insert ((,ceiba-class (:background ,ceiba-vc-insert :foreground ,ceiba-vc-insert))))
   `(diff-hl-delete ((,ceiba-class (:background ,ceiba-vc-delete :foreground ,ceiba-vc-delete))))

   ;; line-number
   `(line-number ((,ceiba-class (:foreground ,ceiba-black))))
   `(line-number-current-line ((,ceiba-class (:foreground ,ceiba-deep-green :background ,ceiba-gray :weight bold))))

   ;; highlight
   `(highlight ((,ceiba-class (:background ,ceiba-gray))))

   ;; shadow
   `(shadow ((,ceiba-class (:foreground ,ceiba-shadow))))

   ;; region
   `(region ((,ceiba-class (:background ,ceiba-gray-blue))))

   ;; mode-line
   `(mode-line ((,ceiba-class (:background ,ceiba-green :foreground ,ceiba-white))))
   `(mode-line-inactive ((,ceiba-class (:background ,ceiba-gray-blue :foreground ,ceiba-white))))
   `(visible-bell ((,ceiba-class (:background ,ceiba-steel-blue :foreground ,ceiba-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,ceiba-class (:foreground ,ceiba-black))))

   ;; borders
   `(vertical-border ((,ceiba-class (:foreground ,ceiba-gray-green))))

   ;; header-line
   `(header-line ((,ceiba-class (:background ,ceiba-green :foreground ,ceiba-white))))
   `(which-func ((,ceiba-class (:background ,ceiba-green :foreground ,ceiba-white))))

   ;; tab-bar
   `(tab-bar ((,ceiba-class (:background ,ceiba-brown-sand :foreground ,ceiba-white))))
   `(tab-bar-tab ((,ceiba-class (:background ,ceiba-green :foreground ,ceiba-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,ceiba-class (:background ,ceiba-brown-sand :foreground ,ceiba-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,ceiba-class (:background ,ceiba-brown-sand :foreground ,ceiba-white))))
   `(tab-line-tab ((,ceiba-class (:background ,ceiba-brown-sand :foreground ,ceiba-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,ceiba-class (:background ,ceiba-green :foreground ,ceiba-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,ceiba-class (:background ,ceiba-brown-sand :foreground ,ceiba-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,ceiba-class (:foreground ,ceiba-purple-red :weight bold :height 0.9))))
   `(tab-line-tab-special ((,ceiba-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,ceiba-class (:foreground ,ceiba-deep-green :weight medium))))
   `(font-lock-string-face ((,ceiba-class (:foreground ,ceiba-light-brown :weight medium))))
   `(font-lock-keyword-face ((,ceiba-class (:foreground ,ceiba-purple :weight medium))))
   `(font-lock-builtin-face ((,ceiba-class (:foreground ,ceiba-alt-blue :weight medium))))
   `(font-lock-warning-face ((,ceiba-class (:foreground ,ceiba-error :weight bold))))
   `(font-lock-type-face ((,ceiba-class (:foreground ,ceiba-green-blue :weight medium))))
   `(font-lock-constant-face ((,ceiba-class (:foreground ,ceiba-orange :weight medium))))
   `(font-lock-function-name-face ((,ceiba-class (:foreground ,ceiba-brown-wood :weight medium))))
   `(font-lock-bracket-face ((,ceiba-class (:weight medium))))
   `(font-lock-variable-name-face ((,ceiba-class (:foreground ,ceiba-green :weight medium))))

   ;; parentheses
   `(show-paren-match ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-deep-green))))
   `(show-paren-mismatch ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,ceiba-class (:background ,ceiba-error))))

   ;; buttons
   `(link ((,ceiba-class (:foreground ,ceiba-purple :underline t :weight bold))))
   `(link-visited ((,ceiba-class (:foreground ,ceiba-purple-red :underline t :weight bold))))
   `(button ((,ceiba-class (:foreground ,ceiba-purple :underline t :weight bold))))


   ;; external packages

   ;; doom-modeline
   `(doom-modeline-project-name ((,ceiba-class (:foreground ,ceiba-steel-blue))))
   `(doom-modeline-project-parent-dir ((,ceiba-class (:foreground ,ceiba-steel-blue))))
   `(doom-modeline-buffer-minor-mode ((,ceiba-class (:foreground ,ceiba-shadow))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,ceiba-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-ceiba)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-ceiba-theme.el ends here
