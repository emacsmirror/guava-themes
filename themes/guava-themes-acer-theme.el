;;; guava-themes-acer-theme.el --- A theme inspired by maple colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 12, 2026
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
;; A theme inspired by maple colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-acer "A theme inspired by maple colors.")

(let* (
      (acer-class '((class color) (min-colors 257)))
      (acer-black             "#000000")
      (acer-white             "#FFFFFF")

      (acer-shadow            "#7f7f7f")

      (acer-green             "#237c34");239834,237c34,3b7b27
      (acer-green-cyan        "#277a6a")
      (acer-deep-green        "#006041");8a8a8a,828282,787878,746c72,747474,606060,60607e,59597e,505069

      (acer-orange            "#e76144");fe7457
      (acer-deep-orange       "#d44400")
      (acer-orange-pink       "#cd475f");ec31a3,ff514a,ff474a,ff475f
      (acer-yellow            "#f7a95a");fca148,fca347,f5a24e,f5a44e,F7B36A,f7a95a
      (acer-autumn            "#ff8c4e");c14c5c,f46157,db3d32,e14337,ed7038,f77b44,f3814f,e8674a,ed674a,f68b47

      (acer-cream             "#dc9964");f68e64,dc8e64,dc9964
      (acer-brown             "#c88550");7d4826,754014,a58464,a07f5f

      (acer-blue              "#2134d5")

      (acer-purple            "#9e4d76")
      (acer-deep-purple       "#60366e");663c6c,62386c
      (acer-purple-red        "#9b234b");9f234b

      (acer-error             "#c80000");FF0000
      (acer-warning           "#f0dc67");F68511,f2e16b
      (acer-success           "#237c34");23D734,239834

      (acer-vc-change         acer-blue)
      (acer-vc-insert         acer-success)
      (acer-vc-delete         acer-error))

  (custom-theme-set-faces
   'guava-themes-acer

   ;; default for acer
   `(default ((,acer-class (:foreground ,acer-black :background ,acer-yellow))))

   ;; error, warning, success
   `(error ((,acer-class (:foreground ,acer-error :weight bold))))
   `(warning ((,acer-class (:foreground ,acer-warning :weight bold))))
   `(success ((,acer-class (:foreground ,acer-success :weight bold))))

   ;; cursor
   `(cursor ((,acer-class (:background ,acer-green :foreground ,acer-black))))

   ;; fringe
   `(fringe ((,acer-class (:background ,acer-yellow :foreground ,acer-black))))
   `(diff-hl-change ((,acer-class (:background ,acer-vc-change :foreground ,acer-vc-change))))
   `(diff-hl-insert ((,acer-class (:background ,acer-vc-insert :foreground ,acer-vc-insert))))
   `(diff-hl-delete ((,acer-class (:background ,acer-vc-delete :foreground ,acer-vc-delete))))

   ;; line-number
   `(line-number ((,acer-class (:foreground ,acer-black))))
   `(line-number-current-line ((,acer-class (:foreground ,acer-deep-orange :background ,acer-cream :weight bold))))

   ;; highlight
   `(highlight ((,acer-class (:background ,acer-cream))))

   ;; shadow
   `(shadow ((,acer-class (:foreground ,acer-shadow))))

   ;; region
   `(region ((,acer-class (:background ,acer-brown))))

   ;; mode-line
   `(mode-line ((,acer-class (:background ,acer-orange :foreground ,acer-white))))
   `(mode-line-inactive ((,acer-class (:background ,acer-purple-red :foreground ,acer-white))))
   `(visible-bell ((,acer-class (:background ,acer-green :foreground ,acer-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,acer-class (:foreground ,acer-black))))

   ;; borders
   `(vertical-border ((,acer-class (:foreground ,acer-yellow))))

   ;; header-line
   `(header-line ((,acer-class (:background ,acer-orange :foreground ,acer-white))))
   `(which-func ((,acer-class (:background ,acer-orange :foreground ,acer-white))))

   ;; tab-bar
   `(tab-bar ((,acer-class (:background ,acer-autumn :foreground ,acer-white))))
   `(tab-bar-tab ((,acer-class (:background ,acer-orange :foreground ,acer-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,acer-class (:background ,acer-autumn :foreground ,acer-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,acer-class (:background ,acer-autumn :foreground ,acer-white))))
   `(tab-line-tab ((,acer-class (:background ,acer-autumn :foreground ,acer-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,acer-class (:background ,acer-orange :foreground ,acer-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,acer-class (:background ,acer-autumn :foreground ,acer-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,acer-class (:foreground ,acer-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,acer-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,acer-class (:foreground ,acer-deep-green :weight medium))))
   `(font-lock-string-face ((,acer-class (:foreground ,acer-deep-orange :weight medium))))
   `(font-lock-keyword-face ((,acer-class (:foreground ,acer-deep-purple :weight medium))))
   `(font-lock-builtin-face ((,acer-class (:foreground ,acer-purple :weight medium))))
   `(font-lock-warning-face ((,acer-class (:foreground ,acer-warning :weight bold))))
   `(font-lock-type-face ((,acer-class (:foreground ,acer-green :weight medium))))
   `(font-lock-constant-face ((,acer-class (:foreground ,acer-green-cyan :weight medium))))
   `(font-lock-function-name-face ((,acer-class (:foreground ,acer-purple-red :weight medium))))
   `(font-lock-bracket-face ((,acer-class (:weight medium))))
   `(font-lock-variable-name-face ((,acer-class (:foreground ,acer-orange-pink :weight medium))))

   ;; parentheses
   `(show-paren-match ((,acer-class (:foreground ,acer-white :background ,acer-green))))
   `(show-paren-mismatch ((,acer-class (:foreground ,acer-white :background ,acer-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,acer-class (:background ,acer-error))))

   ;; buttons
   `(link ((,acer-class (:foreground ,acer-blue :underline t :weight bold))))
   `(link-visited ((,acer-class (:foreground ,acer-green-cyan :underline t :weight bold))))
   `(button ((,acer-class (:foreground ,acer-blue :underline t :weight bold))))


   ;; external packages

   ;; doom-modeline
   `(doom-modeline-project-name ((,acer-class (:foreground ,acer-deep-purple))))
   `(doom-modeline-project-parent-dir ((,acer-class (:foreground ,acer-deep-purple))))
   `(doom-modeline-buffer-minor-mode ((,acer-class (:foreground ,acer-purple))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,acer-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-acer)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-acer-theme.el ends here
