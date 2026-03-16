;;; guava-themes-jacaranda-theme.el --- A theme inspired by jacaranda colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 27, 2025
;; Version: 0.11.5
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
;; A theme inspired by jacaranda colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-jacaranda "A theme inspired by jacaranda colors.")

(let* (
      (jacaranda-class '((class color) (min-colors 257)))
      (jacaranda-black             "#000000")
      (jacaranda-white             "#FFFFFF")

      (jacaranda-shadow            "#7f7f7f")

      (jacaranda-cream             "#e9d9f9");e9e4f9

      (jacaranda-light-green       "#52aa63");52bc63
      (jacaranda-green             "#8ec654")
      (jacaranda-deep-green        "#267a63")
      (jacaranda-oceanic-green     "#10a575");3ab992

      (jacaranda-orange            "#ff9f79");ff9535
      (jacaranda-red               "#ca0036")

      (jacaranda-brown             "#8a7f74")

      (jacaranda-light-blue        "#C0B4E4")
      (jacaranda-blue              "#4534e3")
      (jacaranda-deep-blue         "#655db0")
      (jacaranda-antarctic-blue    "#8d76ca")
      (jacaranda-light-cyan        "#60a1ba");00aaaa
      (jacaranda-cyan              "#00778b")

      (jacaranda-light-purple      "#dbd0fd")
      (jacaranda-purple            "#aa69e6");984ee6
      (jacaranda-deep-purple       "#740cbe");800080
      (jacaranda-purple-red        "#8b2252")

      (jacaranda-error             "#FF0000");FF0000
      (jacaranda-warning           "#ebb515");F68511,ffc333
      (jacaranda-success           "#00c200");228B22,007900

      (jacaranda-vc-change         jacaranda-blue)
      (jacaranda-vc-insert         jacaranda-green)
      (jacaranda-vc-delete         jacaranda-red))

  (custom-theme-set-faces
   'guava-themes-jacaranda

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,jacaranda-class (:foreground ,jacaranda-black :background ,jacaranda-cream))))

   ;; error, warning, success
   `(error ((,jacaranda-class (:foreground ,jacaranda-error :weight bold))))
   `(warning ((,jacaranda-class (:foreground ,jacaranda-warning :weight bold))))
   `(success ((,jacaranda-class (:foreground ,jacaranda-success :weight bold))))

   ;; highlight
   `(highlight ((,jacaranda-class (:background ,jacaranda-light-purple))))

   ;; shadow
   `(shadow ((,jacaranda-class (:foreground ,jacaranda-shadow))))

   ;; region
   `(region ((,jacaranda-class (:background ,jacaranda-light-blue))))
   `(secondary-selection ((,jacaranda-class (:background ,jacaranda-orange :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,jacaranda-class (:foreground ,jacaranda-deep-green :weight medium))))
   `(font-lock-string-face ((,jacaranda-class (:foreground ,jacaranda-purple-red :weight medium))))
   `(font-lock-keyword-face ((,jacaranda-class (:foreground ,jacaranda-deep-purple :weight medium))))
   `(font-lock-builtin-face ((,jacaranda-class (:foreground ,jacaranda-deep-blue :weight medium))))
   `(font-lock-warning-face ((,jacaranda-class (:foreground ,jacaranda-warning :weight bold))))
   `(font-lock-type-face ((,jacaranda-class (:foreground ,jacaranda-oceanic-green :weight medium))))
   `(font-lock-constant-face ((,jacaranda-class (:foreground ,jacaranda-cyan :weight medium))))
   `(font-lock-function-name-face ((,jacaranda-class (:foreground ,jacaranda-blue :weight medium))))
   `(font-lock-punctuation-face ((,jacaranda-class (:foreground ,jacaranda-brown :weight medium))))
   `(font-lock-variable-name-face ((,jacaranda-class (:foreground ,jacaranda-light-cyan :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,jacaranda-class (:background ,jacaranda-purple :foreground ,jacaranda-white))))

   ;; fringe
   `(fringe ((,jacaranda-class (:background ,jacaranda-cream :foreground ,jacaranda-cream))))
   `(diff-hl-change ((,jacaranda-class (:background ,jacaranda-vc-change :foreground ,jacaranda-vc-change))))
   `(diff-hl-insert ((,jacaranda-class (:background ,jacaranda-vc-insert :foreground ,jacaranda-vc-insert))))
   `(diff-hl-delete ((,jacaranda-class (:background ,jacaranda-vc-delete :foreground ,jacaranda-vc-delete))))

   ;; line-number
   `(line-number ((,jacaranda-class (:foreground ,jacaranda-antarctic-blue))))
   `(line-number-current-line ((,jacaranda-class (:foreground ,jacaranda-black :background ,jacaranda-light-purple :weight bold))))

   ;; mode-line
   `(mode-line ((,jacaranda-class (:background ,jacaranda-deep-blue :foreground ,jacaranda-white))))
   `(mode-line-inactive ((,jacaranda-class (:background ,jacaranda-light-blue :foreground ,jacaranda-white))))
   `(guava-themes-visible-bell ((,jacaranda-class (:background ,jacaranda-orange :foreground ,jacaranda-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,jacaranda-class (:foreground ,jacaranda-black))))

   ;; borders
   `(vertical-border ((,jacaranda-class (:foreground ,jacaranda-light-purple))))

   ;; header-line
   `(header-line ((,jacaranda-class (:background ,jacaranda-deep-blue :foreground ,jacaranda-white))))
   `(which-func ((,jacaranda-class (:background ,jacaranda-deep-blue :foreground ,jacaranda-white))))

   ;; tab-bar
   `(tab-bar ((,jacaranda-class (:background ,jacaranda-purple :foreground ,jacaranda-white))))
   `(tab-bar-tab ((,jacaranda-class (:background ,jacaranda-deep-blue :foreground ,jacaranda-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,jacaranda-class (:background ,jacaranda-purple :foreground ,jacaranda-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,jacaranda-class (:background ,jacaranda-purple :foreground ,jacaranda-white))))
   `(tab-line-tab ((,jacaranda-class (:background ,jacaranda-purple :foreground ,jacaranda-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,jacaranda-class (:background ,jacaranda-deep-blue :foreground ,jacaranda-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,jacaranda-class (:background ,jacaranda-purple :foreground ,jacaranda-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,jacaranda-class (:foreground ,jacaranda-orange :weight bold :height 0.9))))
   `(tab-line-tab-special ((,jacaranda-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-purple))))
   `(show-paren-mismatch ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,jacaranda-class (:background ,jacaranda-error))))

   ;; buttons
   `(link ((,jacaranda-class (:foreground ,jacaranda-oceanic-green :underline t :weight bold))))
   `(link-visited ((,jacaranda-class (:foreground ,jacaranda-deep-green :underline t :weight bold))))
   `(button ((,jacaranda-class (:foreground ,jacaranda-blue :underline t :weight bold))))


   ;; external packages

   ;; doom-modeline
   `(doom-modeline-project-name ((,jacaranda-class (:foreground ,jacaranda-light-green))))
   `(doom-modeline-project-parent-dir ((,jacaranda-class (:foreground ,jacaranda-light-green))))
   `(doom-modeline-buffer-minor-mode ((,jacaranda-class (:foreground ,jacaranda-light-blue))))

   ;; corfu
   `(corfu-default ((,jacaranda-class (:foreground ,jacaranda-black :background ,jacaranda-cream))))
   `(corfu-current ((,jacaranda-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,jacaranda-class (:background ,jacaranda-shadow))))
   `(corfu-border ((,jacaranda-class (:background ,jacaranda-shadow))))

   ;; envrc
   `(envrc-mode-line-error-face ((,jacaranda-class (:inherit error))))
   `(envrc-mode-line-none-face ((,jacaranda-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,jacaranda-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,jacaranda-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-jacaranda)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-jacaranda-theme.el ends here
