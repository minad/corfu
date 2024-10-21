;;; corfu-pixel-perfect.el --- Pixel perfect Corfu -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Created: 2024
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (corfu "1.6"))
;; URL: https://github.com/minad/corfu
;; Keywords: abbrev, convenience, matching, completion, text

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Corfu by default measures string widths in columns. This extension measures
;; strings in pixels to support uneven glyph widths such as the case with emoji
;; , faces using variable fonts, display text properties etc.

;;; Code:

(require 'corfu)
(require 'mule-util)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(cl-defmethod corfu--string-width (str &context (corfu-pixel-perfect-mode (eql t)))
  "Measure the width of string STR in pixels."
  (string-pixel-width str))

(cl-defmethod corfu--truncate-string-to-width
  (str width &context (corfu-pixel-perfect-mode (eql t)))
  "Truncate string STR to WIDTH.
WIDTH is in pixels. If the string is longer than width when
rendered, it is truncated with the last character(s) replaced
with the result of `truncate-string-ellipsis'. If shorter,
returns an empty string."
  (let* ((glyphs (string-glyph-split str))
         (glyph-width (string-pixel-width (car glyphs)))
         (face (and glyphs (get-text-property 0 'face (car (last glyphs)))))
         (ellipsis (apply 'propertize (truncate-string-ellipsis) (if face `(face ,face))))
         (ellipsis-width (string-pixel-width ellipsis))
         result)
    (while (and glyphs (<= glyph-width width))
      (setq result (cons (car glyphs) result)
            glyphs (cdr glyphs)
            width (- width glyph-width)
            glyph-width (string-pixel-width (car glyphs))))

    (when (and glyphs result) ;; truncated
      (while (and result (> ellipsis-width width))
        (setq glyphs (cons (car result) glyphs)
              result (cdr result)
              width (+ width glyph-width)
              glyph-width (string-pixel-width (car result))))
      (setq result (cons ellipsis result)))

    (string-join (nreverse result))))

;;;###autoload
(define-minor-mode corfu-pixel-perfect-mode
  "Pixel perfect Corfu."
  :global t
  :group 'corfu
  (setq corfu--pixel-wise corfu-pixel-perfect-mode))

(provide 'corfu-pixel-perfect)
;;; corfu-pixel-perfect.el ends here
