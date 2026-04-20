;;; guava-themes-petunia-theme.el --- A theme inspired by petunia colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Apr 19, 2026
;; Version: 0.15.0
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
;; A theme inspired by petunia colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-petunia "A theme inspired by petunia colors.")

(let* (
      (petunia-class '((class color) (min-colors 257)))
      (petunia-black                     "#000000")
      (petunia-white                     "#FFFFFF")

      (petunia-red                       "#e60046");dc2850
      (petunia-pink                      "#be64aa");b40bc0,b44bc0
      (petunia-cream                     "#ffa0a0")
      (petunia-light-orange              "#ffb478");ffa07a
      (petunia-orange                    "#ff7332");ff6432

      (petunia-yellow                    "#fafd70")

      (petunia-light-green               "#78c878");6eb46e
      (petunia-green-forest              "#008741");169b2f

      (petunia-light-blue                "#00afd7")
      (petunia-alt-light-blue            "#328ccd")
      (petunia-blue                      "#3232cd")
      (petunia-deep-blue                 "#004b82");323296
      (petunia-cyan                      "#00ffff")

      (petunia-light-purple              "#a88dac")
      (petunia-purple                    "#8a46ff");8a14fd,8a14e1,8a1eff,8a28ff

      (petunia-fg                        "#FFFFFF")
      (petunia-bg                        "#000000")
      (petunia-highlight                 "#1e1e1e")
      (petunia-shadow                    "#b3b3b3")

      (petunia-error                     "#FF0000")
      (petunia-warning                   "#ffff00");f6d909
      (petunia-success                   "#00ff00");1ebe1e

      (petunia-diff-added                "#5aa05a");335533
      (petunia-diff-removed              "#a05a5a");553333
      (petunia-diff-refine-added         "#007800");22aa22
      (petunia-diff-refine-removed       "#780000");aa2222
      (petunia-diff-header               "#5a5a5a");737373
      (petunia-diff-file-header          "#3c3c3c");999999
      (petunia-diff-context              "#828282");999999

      (petunia-orderless-0               "#c878dc");af50c8
      (petunia-orderless-1               "#28b43c");28a03c
      (petunia-orderless-2               "#ff7300");ff6400
      (petunia-orderless-3               "#009bff");3c82ff

      (petunia-vc-change                 petunia-blue)
      (petunia-vc-insert                 petunia-success)
      (petunia-vc-delete                 petunia-error))

  (custom-theme-set-faces
   'guava-themes-petunia

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,petunia-class (:foreground ,petunia-fg :background ,petunia-bg))))

   ;; error, warning, success
   `(error ((,petunia-class (:foreground ,petunia-error :weight bold))))
   `(warning ((,petunia-class (:foreground ,petunia-warning :weight bold))))
   `(success ((,petunia-class (:foreground ,petunia-success :weight bold))))

   ;; highlight
   `(highlight ((,petunia-class (:background ,petunia-highlight))))

   ;; shadow
   `(shadow ((,petunia-class (:foreground ,petunia-shadow))))

   ;; region
   `(region ((,petunia-class (:background ,petunia-blue))))
   `(secondary-selection ((,petunia-class (:background ,petunia-alt-light-blue :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,petunia-class (:foreground ,petunia-light-green :weight medium))))
   `(font-lock-string-face ((,petunia-class (:foreground ,petunia-orange :weight medium))))
   `(font-lock-keyword-face ((,petunia-class (:foreground ,petunia-purple :weight medium))))
   `(font-lock-builtin-face ((,petunia-class (:foreground ,petunia-light-blue :weight medium))))
   `(font-lock-warning-face ((,petunia-class (:foreground ,petunia-warning :weight bold))))
   `(font-lock-type-face ((,petunia-class (:foreground ,petunia-yellow :weight medium))))
   `(font-lock-constant-face ((,petunia-class (:foreground ,petunia-green-forest :weight medium))))
   `(font-lock-function-name-face ((,petunia-class (:foreground ,petunia-red :weight medium))))
   `(font-lock-punctuation-face ((,petunia-class (:foreground ,petunia-light-purple :weight medium))))
   `(font-lock-variable-name-face ((,petunia-class (:foreground ,petunia-pink :weight medium))))
   `(font-lock-negation-char-face ((,petunia-class (:foreground ,petunia-light-orange :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,petunia-class (:foreground ,petunia-black :background ,petunia-light-blue))))

   ;; fringe
   `(fringe ((,petunia-class (:foreground ,petunia-red :background ,petunia-bg))))
   `(diff-hl-change ((,petunia-class (:foreground ,petunia-vc-change :background ,petunia-vc-change))))
   `(diff-hl-insert ((,petunia-class (:foreground ,petunia-vc-insert :background ,petunia-vc-insert))))
   `(diff-hl-delete ((,petunia-class (:foreground ,petunia-vc-delete :background ,petunia-vc-delete))))

   ;; line-number
   `(line-number ((,petunia-class (:foreground ,petunia-fg))))
   `(line-number-current-line ((,petunia-class (:foreground ,petunia-light-orange :weight bold :inherit highlight))))
   `(line-number-minor-tick ((,petunia-class (:background ,petunia-light-blue :inherit line-number))))
   `(line-number-major-tick ((,petunia-class (:background ,petunia-blue :inherit line-number))))

   ;; mode-line
   `(mode-line ((,petunia-class (:foreground ,petunia-white :background ,petunia-purple))))
   `(mode-line-inactive ((,petunia-class (:foreground ,petunia-white :background ,petunia-orange))))
   `(guava-themes-visible-bell ((,petunia-class (:foreground ,petunia-white :background ,petunia-light-green))))

   ;; minibuffer
   `(minibuffer-prompt ((,petunia-class (:foreground ,petunia-orange))))

   ;; borders
   `(vertical-border ((,petunia-class (:foreground ,petunia-purple))))

   ;; header-line
   `(header-line ((,petunia-class (:foreground ,petunia-white :background ,petunia-purple))))
   `(which-func ((,petunia-class (:foreground ,petunia-white))))

   ;; tab-bar
   `(tab-bar ((,petunia-class (:foreground ,petunia-white :background ,petunia-green-forest))))
   `(tab-bar-tab ((,petunia-class (:foreground ,petunia-white :background ,petunia-purple :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,petunia-class (:foreground ,petunia-white :background ,petunia-light-green :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,petunia-class (:foreground ,petunia-white :background ,petunia-green-forest))))
   `(tab-line-tab ((,petunia-class (:foreground ,petunia-white :background ,petunia-orange :weight bold :height 0.9))))
   `(tab-line-tab-current ((,petunia-class (:foreground ,petunia-white :background ,petunia-purple :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,petunia-class (:foreground ,petunia-white :background ,petunia-green-forest :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,petunia-class (:foreground ,petunia-white :background ,petunia-light-green :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,petunia-class (:foreground ,petunia-orange :weight bold :height 0.9))))
   `(tab-line-tab-special ((,petunia-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,petunia-class (:foreground ,petunia-black :background ,petunia-light-orange))))
   `(show-paren-mismatch ((,petunia-class (:foreground ,petunia-white :background ,petunia-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,petunia-class (:background ,petunia-error))))

   ;; links
   `(link ((,petunia-class (:foreground ,petunia-light-blue :underline t :weight bold))))
   `(link-visited ((,petunia-class (:foreground ,petunia-blue :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,petunia-class (:foreground ,petunia-orange :weight medium))))
   `(outline-2 ((,petunia-class (:foreground ,petunia-light-green :weight medium))))
   `(outline-3 ((,petunia-class (:foreground ,petunia-pink :weight medium))))
   `(outline-4 ((,petunia-class (:foreground ,petunia-blue :weight medium))))
   `(outline-5 ((,petunia-class (:foreground ,petunia-red :weight medium))))
   `(outline-6 ((,petunia-class (:foreground ,petunia-green-forest :weight medium))))
   `(outline-7 ((,petunia-class (:foreground ,petunia-yellow :weight medium))))
   `(outline-8 ((,petunia-class (:foreground ,petunia-purple :weight medium))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,petunia-class (:foreground ,petunia-cyan))))
   `(escape-glyph ((,petunia-class (:inherit homoglyph))))
   `(nobreak-space ((,petunia-class (:box (:line-width (2 . 2)) :inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,petunia-class (:background ,petunia-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,petunia-class (:foreground ,petunia-cyan :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-header :extend t))))
   `(diff-file-header ((,petunia-class (:weight bold :foreground ,petunia-white :background ,petunia-diff-file-header :extend t))))
   `(diff-context ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-context :extend t))))

   ;; completions
   `(completions-common-part ((,petunia-class (:foreground ,petunia-warning :weight bold))))
   `(completions-first-difference ((,petunia-class (:foreground ,petunia-error :weight bold))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,petunia-class (:foreground ,petunia-pink))))
   `(elfeed-search-date-face ((,petunia-class (:foreground ,petunia-light-blue))))
   `(elfeed-search-feed-face ((,petunia-class (:foreground ,petunia-green-forest))))
   `(elfeed-search-title-face ((,petunia-class (:foreground ,petunia-deep-blue))))
   `(elfeed-search-filter-face ((,petunia-class (:weight bold :foreground ,petunia-yellow))))
   `(elfeed-search-last-update-face ((,petunia-class (:weight bold :foreground ,petunia-light-green))))
   `(elfeed-search-unread-title-face ((,petunia-class (:weight bold :foreground ,petunia-purple))))
   `(elfeed-search-unread-count-face ((,petunia-class (:weight bold :foreground ,petunia-light-orange))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,petunia-class (:foreground ,petunia-light-green :inherit italic))))
   `(doom-modeline-project-parent-dir ((,petunia-class (:foreground ,petunia-light-green))))
   `(doom-modeline-buffer-minor-mode ((,petunia-class (:foreground ,petunia-yellow))))

   ;; corfu
   `(corfu-default ((,petunia-class (:foreground ,petunia-fg :background ,petunia-bg))))
   `(corfu-current ((,petunia-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,petunia-class (:background ,petunia-shadow))))
   `(corfu-border ((,petunia-class (:background ,petunia-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,petunia-class (:foreground ,petunia-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,petunia-class (:foreground ,petunia-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,petunia-class (:foreground ,petunia-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,petunia-class (:foreground ,petunia-orderless-3 :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,petunia-class (:inherit error))))
   `(envrc-mode-line-none-face ((,petunia-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,petunia-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,petunia-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-petunia)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-petunia-theme.el ends here
