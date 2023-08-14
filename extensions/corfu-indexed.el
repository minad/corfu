;;; corfu-indexed.el --- Select indexed candidates -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author: Luis Henriquez-Perez <luis@luishp.xyz>, Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (corfu "0.38"))
;; Homepage: https://github.com/minad/corfu

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

;; This package is a Corfu extension, which prefixes candidates with indices if
;; enabled via `corfu-indexed-mode'.  It allows you to select candidates with
;; prefix arguments.  This is designed to be a faster alternative to selecting a
;; candidate with `corfu-next' and `corfu-previous'.

;;; Code:

(require 'corfu)
(eval-when-compile
  (require 'cl-lib))

(defface corfu-indexed
  '((default :height 0.75)
    (((class color) (min-colors 88) (background dark))
     :foreground "#f4f4f4" :background "#323232")
     (((class color) (min-colors 88) (background light))
     :foreground "#404148" :background "#d7d7d7")
    (t :background "black"))
  "Face used for the candidate index prefix."
  :group 'corfu-faces)

(defcustom corfu-indexed-start 0
  "Start of the indexing."
  :group 'corfu
  :type 'natnum)

(defvar corfu-indexed--commands
  '(corfu-insert corfu-complete)
  "Commands that should be indexed.")

;;;###autoload
(define-minor-mode corfu-indexed-mode
  "Prefix candidates with indices."
  :global t :group 'corfu)

(cl-defmethod corfu--prepare :before (&context (corfu-indexed-mode (eql t)))
  (when (and prefix-arg (memq this-command corfu-indexed--commands))
    (let ((index (+ corfu--scroll
                    (- (prefix-numeric-value prefix-arg)
                       corfu-indexed-start))))
      (if (and (>= index 0)
               (< index corfu--total)
               (< index (+ corfu--scroll corfu-count)))
          (setq corfu--index index)
        (message "Out of range")
        (setq this-command #'ignore)))))

(cl-defmethod corfu--affixate :around (cands &context (corfu-indexed-mode (eql t)))
  (setq cands (cdr (cl-call-next-method cands)))
  (let* ((space #(" " 0 1 (face (:height 0.5 :inherit corfu-indexed))))
         (width (if (length> cands (- 10 corfu-indexed-start)) 2 1))
         (fmt (concat space
                      (propertize (format "%%%ds" width)
                                  'face 'corfu-indexed)
                      space))
         (align
          (propertize (make-string width ?\s)
                      'display
                      `(space :align-to (+ left ,(1+ width))))))
    (cl-loop for cand in cands for index from corfu-indexed-start do
      (setf (cadr cand)
            (concat
             (propertize " " 'display (format fmt index))
             align
             (cadr cand))))
    (cons t cands)))

(provide 'corfu-indexed)
;;; corfu-indexed.el ends here
