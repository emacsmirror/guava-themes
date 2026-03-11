;;; guava-themes-prunus-theme.el --- A theme inspired by cherry colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 29, 2025
;; Version: 0.11.4
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
;; A theme inspired by cherry colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-prunus "A theme inspired by cherry colors.")

(let* (
      (prunus-class '((class color) (min-colors 257)))
      ;;(prunus-black             "#000000")
      (prunus-white             "#FFFFFF")

      (prunus-shadow            "#b3b3b3")

      (prunus-cream             "#DEA2BD");fffef5,EBDCF5
      (prunus-light-brown       "#735944")
      (prunus-brown             "#4A301B");583c25,
      (prunus-dark-brown        "#1A0E05");E9E4F9,3F271D,2E1E03,281A04,1A0E05,1C0E06,1C0F07

      (prunus-light-green       "#52BC63")
      (prunus-oceanic-green     "#3AC3A2")

      (prunus-deep-orange       "#C46935");a0522d
      (prunus-red               "#88190C");cb001e,d2191e
      (prunus-pink              "#CD2788");dc6199,cd4f88

      (prunus-blue              "#4534E3");4534e3,120cdc
      (prunus-deep-blue         "#655DB0")
      (prunus-antarctic-blue    "#8D76CA")
      (prunus-cyan              "#008B8B")

      (prunus-deep-purple       "#740CBE");800080
      (prunus-purple-red        "#8B2252")

      (prunus-error             "#FF0000");FF0000,bc0000,890014
      (prunus-warning           "#f6d911");F68511,ffc333,F68511
      (prunus-success           "#23D734");228B22,007900

      (prunus-vc-change         prunus-blue)
      (prunus-vc-insert         prunus-success)
      (prunus-vc-delete         prunus-error))

  (custom-theme-set-faces
   'guava-themes-prunus

   ;; default for prunus
   `(default ((,prunus-class (:foreground ,prunus-cream :background ,prunus-dark-brown))))

   ;; error, warning, success
   `(error ((,prunus-class (:foreground ,prunus-error :weight bold))))
   `(warning ((,prunus-class (:foreground ,prunus-warning :weight bold))))
   `(success ((,prunus-class (:foreground ,prunus-success :weight bold))))

   ;; cursor
   `(cursor ((,prunus-class (:background ,prunus-deep-blue :foreground ,prunus-white))))

   ;; fringe
   `(fringe ((,prunus-class (:background ,prunus-dark-brown :foreground ,prunus-cream))))
   `(diff-hl-change ((,prunus-class (:background ,prunus-vc-change :foreground ,prunus-vc-change))))
   `(diff-hl-insert ((,prunus-class (:background ,prunus-vc-insert :foreground ,prunus-vc-insert))))
   `(diff-hl-delete ((,prunus-class (:background ,prunus-vc-delete :foreground ,prunus-vc-delete))))

   ;; line-number
   `(line-number ((,prunus-class (:foreground ,prunus-purple-red))))
   `(line-number-current-line ((,prunus-class (:foreground ,prunus-cream :background ,prunus-brown :weight bold))))

   ;; highlight
   `(highlight ((,prunus-class (:background ,prunus-brown))))

   ;; shadow
   `(shadow ((,prunus-class (:foreground ,prunus-shadow))))

   ;; region
   `(region ((,prunus-class (:background ,prunus-light-brown))))

   ;; mode-line
   `(mode-line ((,prunus-class (:background ,prunus-red :foreground ,prunus-white))))
   `(mode-line-inactive ((,prunus-class (:background ,prunus-deep-orange :foreground ,prunus-white))))
   `(guava-themes-visible-bell ((,prunus-class (:background ,prunus-antarctic-blue :foreground ,prunus-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,prunus-class (:foreground ,prunus-white))))

   ;; borders
   `(vertical-border ((,prunus-class (:foreground ,prunus-pink))))

   ;; header-line
   `(header-line ((,prunus-class (:background ,prunus-red :foreground ,prunus-white))))
   `(which-func ((,prunus-class (:background ,prunus-red :foreground ,prunus-white))))

   ;; tab-bar
   `(tab-bar ((,prunus-class (:background ,prunus-pink :foreground ,prunus-white))))
   `(tab-bar-tab ((,prunus-class (:background ,prunus-red :foreground ,prunus-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,prunus-class (:background ,prunus-pink :foreground ,prunus-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,prunus-class (:background ,prunus-pink :foreground ,prunus-white))))
   `(tab-line-tab ((,prunus-class (:background ,prunus-pink :foreground ,prunus-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,prunus-class (:background ,prunus-red :foreground ,prunus-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,prunus-class (:background ,prunus-pink :foreground ,prunus-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,prunus-class (:foreground ,prunus-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,prunus-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,prunus-class (:foreground ,prunus-light-green :weight medium))))
   `(font-lock-string-face ((,prunus-class (:foreground ,prunus-purple-red :weight medium))))
   `(font-lock-keyword-face ((,prunus-class (:foreground ,prunus-deep-purple :weight medium))))
   `(font-lock-builtin-face ((,prunus-class (:foreground ,prunus-blue :weight medium))))
   `(font-lock-warning-face ((,prunus-class (:foreground ,prunus-warning :weight bold))))
   `(font-lock-type-face ((,prunus-class (:foreground ,prunus-antarctic-blue :weight medium))))
   `(font-lock-constant-face ((,prunus-class (:foreground ,prunus-cyan :weight medium))))
   `(font-lock-function-name-face ((,prunus-class (:foreground ,prunus-deep-blue :weight medium))))
   `(font-lock-bracket-face ((,prunus-class (:weight medium))))
   `(font-lock-variable-name-face ((,prunus-class (:foreground ,prunus-deep-orange :weight medium))))

   ;; parentheses
   `(show-paren-match ((,prunus-class (:foreground ,prunus-white :background ,prunus-oceanic-green))))
   `(show-paren-mismatch ((,prunus-class (:foreground ,prunus-white :background ,prunus-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,prunus-class (:background ,prunus-error))))

   ;; buttons
   `(link ((,prunus-class (:foreground ,prunus-blue :underline t :weight bold))))
   `(link-visited ((,prunus-class (:foreground ,prunus-oceanic-green :underline t :weight bold))))
   `(button ((,prunus-class (:foreground ,prunus-blue :underline t :weight bold))))


   ;; external packages

   ;; doom-modeline
   `(doom-modeline-project-name ((,prunus-class (:foreground ,prunus-light-green))))
   `(doom-modeline-project-parent-dir ((,prunus-class (:foreground ,prunus-light-green))))
   `(doom-modeline-buffer-minor-mode ((,prunus-class (:foreground ,prunus-shadow))))

   ;; corfu
   `(corfu-default ((,prunus-class (:foreground ,prunus-cream :background ,prunus-dark-brown))))
   `(corfu-current ((,prunus-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,prunus-class (:background ,prunus-shadow))))
   `(corfu-border ((,prunus-class (:background ,prunus-shadow))))

   ;; envrc
   `(envrc-mode-line-error-face ((,prunus-class (:inherit error))))
   `(envrc-mode-line-none-face ((,prunus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,prunus-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,prunus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-prunus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-prunus-theme.el ends here
