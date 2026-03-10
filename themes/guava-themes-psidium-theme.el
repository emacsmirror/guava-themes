;;; guava-themes-psidium-theme.el --- A theme inspired by guava colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 07, 2025
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
;; A theme inspired by guava colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-psidium "A theme inspired by guava colors.")

(let* (
      (psidium-class '((class color) (min-colors 257)))
      (psidium-black             "#000000")
      (psidium-white             "#FFFFFF")

      (psidium-shadow            "#7f7f7f")

      (psidium-cream             "#F1EECE");F7DEB6

      (psidium-green             "#599F48")
      (psidium-light-green       "#28bb6b");13765e,13845e,139570,3ab488,3ab992
      (psidium-guava-green       "#AED734");AECA41,AED234
      (psidium-deep-green        "#097d2c");09662c

      (psidium-orange            "#ffa55f");FF7D5F,ff8f5f
      (psidium-red               "#c1153b");f02d1b
      (psidium-light-pink        "#FCD0C9");F8917C
      (psidium-pink              "#F8767C");F84865,F8767C,F88686,F85A65

      (psidium-brown             "#816249");7D5E45

      (psidium-light-blue        "#41C3CA")
      (psidium-oceanic-blue      "#2a4ad9")
      (psidium-deep-blue         "#483d8b");004F5D

      (psidium-purple            "#812db2");D7137C,C0137C,B00CE0,a62db2,8e2db2

      (psidium-error             "#FF0000")
      (psidium-warning           "#dfe300");FF8C00,f08020,f68511
      (psidium-success           "#228B22")

      (psidium-vc-change         psidium-light-blue)
      (psidium-vc-insert         psidium-green)
      (psidium-vc-delete         psidium-error))

  (custom-theme-set-faces
   'guava-themes-psidium

   ;; default for psidium
   `(default ((,psidium-class (:foreground ,psidium-black :background ,psidium-cream))))

   ;; error, warning, success
   `(error ((,psidium-class (:foreground ,psidium-error :weight bold))))
   `(warning ((,psidium-class (:foreground ,psidium-warning :weight bold))))
   `(success ((,psidium-class (:foreground ,psidium-success :weight bold))))

   ;; cursor
   `(cursor ((,psidium-class (:background ,psidium-green :foreground ,psidium-white))))

   ;; fringe
   `(fringe ((,psidium-class (:background ,psidium-cream :foreground ,psidium-cream))))
   `(diff-hl-change ((,psidium-class (:background ,psidium-vc-change :foreground ,psidium-vc-change))))
   `(diff-hl-insert ((,psidium-class (:background ,psidium-vc-insert :foreground ,psidium-vc-insert))))
   `(diff-hl-delete ((,psidium-class (:background ,psidium-vc-delete :foreground ,psidium-vc-delete))))

   ;; line-number
   `(line-number ((,psidium-class (:foreground ,psidium-brown))))
   `(line-number-current-line ((,psidium-class (:foreground ,psidium-black :background ,psidium-light-pink :weight bold))))

   ;; highlight
   `(highlight ((,psidium-class (:background ,psidium-light-pink))))

   ;; shadow
   `(shadow ((,psidium-class (:foreground ,psidium-shadow))))

   ;; region
   `(region ((,psidium-class (:background ,psidium-orange))))

   ;; mode-line
   `(mode-line ((,psidium-class (:background ,psidium-pink :foreground ,psidium-white))))
   `(mode-line-inactive ((,psidium-class (:background ,psidium-green :foreground ,psidium-white))))
   `(visible-bell ((,psidium-class (:background ,psidium-deep-green :foreground ,psidium-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,psidium-class (:foreground ,psidium-black))))

   ;;borders
   `(vertical-border ((,psidium-class (:foreground ,psidium-cream))))

   ;; header-line
   `(header-line ((,psidium-class (:background ,psidium-pink :foreground ,psidium-white))))
   `(which-func ((,psidium-class (:background ,psidium-pink :foreground ,psidium-white))))

   ;; tab-bar
   `(tab-bar ((,psidium-class (:background ,psidium-guava-green :foreground ,psidium-white))))
   `(tab-bar-tab ((,psidium-class (:background ,psidium-pink :foreground ,psidium-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,psidium-class (:background ,psidium-guava-green :foreground ,psidium-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,psidium-class (:background ,psidium-guava-green :foreground ,psidium-white))))
   `(tab-line-tab ((,psidium-class (:background ,psidium-guava-green :foreground ,psidium-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,psidium-class (:background ,psidium-pink :foreground ,psidium-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,psidium-class (:background ,psidium-guava-green :foreground ,psidium-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,psidium-class (:foreground ,psidium-deep-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,psidium-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,psidium-class (:foreground ,psidium-green :weight medium))))
   `(font-lock-string-face ((,psidium-class (:foreground ,psidium-brown :weight medium))))
   `(font-lock-keyword-face ((,psidium-class (:foreground ,psidium-red :weight medium))))
   `(font-lock-builtin-face ((,psidium-class (:foreground ,psidium-deep-blue :weight medium))))
   `(font-lock-warning-face ((,psidium-class (:foreground ,psidium-error :weight bold))))
   `(font-lock-type-face ((,psidium-class (:foreground ,psidium-deep-green :weight medium))))
   `(font-lock-constant-face ((,psidium-class (:foreground ,psidium-oceanic-blue :weight medium))))
   `(font-lock-function-name-face ((,psidium-class (:foreground ,psidium-light-green :weight medium))))
   `(font-lock-bracket-face ((,psidium-class (:weight medium))))
   `(font-lock-variable-name-face ((,psidium-class (:foreground ,psidium-purple :weight medium))))

   ;; parentheses
   `(show-paren-match ((,psidium-class (:foreground ,psidium-white :background ,psidium-orange))))
   `(show-paren-mismatch ((,psidium-class (:foreground ,psidium-white :background ,psidium-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,psidium-class (:background ,psidium-error))))

   ;; buttons
   `(link ((,psidium-class (:foreground ,psidium-light-blue :underline t :weight bold))))
   `(link-visited ((,psidium-class (:foreground ,psidium-purple :underline t :weight bold))))
   `(button ((,psidium-class (:foreground ,psidium-light-blue :underline t :weight bold))))


   ;; external packages

   ;; doom-modeline
   `(doom-modeline-project-name ((,psidium-class (:foreground ,psidium-deep-blue))))
   `(doom-modeline-project-parent-dir ((,psidium-class (:foreground ,psidium-deep-blue))))
   `(doom-modeline-buffer-minor-mode ((,psidium-class (:foreground ,psidium-shadow))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,psidium-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-psidium)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-psidium-theme.el ends here
