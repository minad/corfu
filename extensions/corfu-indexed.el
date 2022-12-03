;;; corfu-indexed.el --- Select indexed candidates -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Luis Henriquez-Perez <luis@luishp.xyz>, Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (corfu "0.34"))
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
;; enabled via `corfu-indexed-mode'. It allows you to select candidates with
;; prefix arguments. This is designed to be a faster alternative to selecting a
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
  :type 'integer)

(defvar corfu-indexed--commands
  '(corfu-insert corfu-complete)
  "Commands that should be indexed.")

(defun corfu-indexed--affixate (cands)
  "Advice for `corfu--affixate' which prefixes the CANDS with an index."
  (setq cands (cdr cands))
  (let* ((space #(" " 0 1 (face (:height 0.5 :inherit corfu-indexed))))
         (width (if (> (+ corfu-indexed-start (length cands)) 10) 2 1))
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

(defun corfu-indexed--handle-prefix (orig &rest args)
  "Handle prefix argument before calling ORIG function with ARGS."
  (if (and current-prefix-arg (called-interactively-p t))
      (let ((corfu--index (+ corfu--scroll
                             (- (prefix-numeric-value current-prefix-arg)
                                corfu-indexed-start))))
        (if (or (< corfu--index 0)
                (>= corfu--index corfu--total)
                (>= corfu--index (+ corfu--scroll corfu-count)))
            (message "Out of range")
          (funcall orig)))
    (apply orig args)))

;;;###autoload
(define-minor-mode corfu-indexed-mode
  "Prefix candidates with indices."
  :global t :group 'corfu
  (cond
   (corfu-indexed-mode
    (advice-add #'corfu--affixate :filter-return #'corfu-indexed--affixate)
    (dolist (cmd corfu-indexed--commands)
      (advice-add cmd :around #'corfu-indexed--handle-prefix)))
   (t
    (advice-remove #'corfu--affixate #'corfu-indexed--affixate)
    (dolist (cmd corfu-indexed--commands)
      (advice-remove cmd #'corfu-indexed--handle-prefix)))))

(provide 'corfu-indexed)
;;; corfu-indexed.el ends here
