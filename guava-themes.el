;;; guava-themes.el --- A pack of guava-colored themes for Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 07, 2025
;; Version: 0.4.0
;; Keywords: themes, faces, color
;; URL: http://github.com/bormoge/guava-themes.el
;; Package-Requires: ((emacs "29.1"))
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
;; A bunch of guava-inspired themes for GNU Emacs.
;;
;;; Code:

(unless (>= emacs-major-version 29)
  (error "A version of Emacs equal or superior to 29 is required"))

(defgroup guava nil
  "Guava theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defface guava-visible-bell '()
  "Face to use as a replacement for `visible-bell'."
  :group 'guava)

(defun guava-change-visible-bell ()
  "Change the blink of the minibuffer with a blink for the mode-line.
Set `ring-bell-function' with this function as its value to use it."
  (let* ((buf (current-buffer))
         (faces (if (facep 'mode-line-active)
                    '(mode-line-active)
                 '(mode-line)))
         (cookies (mapcar (lambda (face)
                            (when (facep face)
                              (face-remap-add-relative face 'guava-visible-bell)))
                          faces)))
    (force-mode-line-update)
    (run-with-timer 0.15 nil
                    (lambda ()
                      (with-current-buffer buf
                        (mapc #'face-remap-remove-relative cookies)
                        (force-mode-line-update))))))

(provide 'guava-themes)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-themes.el ends here
