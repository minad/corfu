;;; corfu-quick.el --- Quick keys for Corfu -*- lexical-binding: t -*-

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

;; This package is a Corfu extension, which prefixes candidates with
;; quick keys.  Typing these quick keys allows you to select the
;; candidate in front of them.  This is designed to be a faster
;; alternative to selecting a candidate with `corfu-next' and
;; `corfu-previous'.
;; (keymap-set corfu-map "M-q" #'corfu-quick-complete)
;; (keymap-set corfu-map "C-q" #'corfu-quick-insert)

;;; Code:

(require 'corfu)
(eval-when-compile
  (require 'cl-lib))

(defcustom corfu-quick1 "asdfgh"
  "First level quick keys."
  :type 'string
  :group 'corfu)

(defcustom corfu-quick2 "jkluionm"
  "Second level quick keys."
  :type 'string
  :group 'corfu)

(defface corfu-quick1
  '((((class color) (min-colors 88) (background dark))
     :background "#0050af" :foreground "white" :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#7feaff" :foreground "black" :inherit bold)
    (t :background "blue" :foreground "white" :inherit bold))
  "Face used for the first quick key."
  :group 'corfu-faces)

(defface corfu-quick2
  '((((class color) (min-colors 88) (background dark))
     :background "#7f1f7f" :foreground "white" :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#ffaaff" :foreground "black" :inherit bold)
    (t :background "magenta" :foreground "white" :inherit bold))
  "Face used for the second quick key."
  :group 'corfu-faces)

(defun corfu-quick--keys (two idx) ;; See vertico-quick--keys
  "Format quick keys prefix.
IDX is the current candidate index.
TWO is non-nil if two keys should be displayed."
  (let ((fst (length corfu-quick1))
        (snd (length corfu-quick2)))
    (if (>= idx fst)
        (let ((first (elt corfu-quick2 (mod (/ (- idx fst) fst) snd)))
              (second (elt corfu-quick1 (mod (- idx fst) fst))))
          (cond
           ((eq first two)
            (list
             (concat " " (propertize (char-to-string second) 'face 'corfu-quick1))
             (cons second (+ corfu--scroll idx))))
           (two
            (list "  "))
           (t
            (list
             (concat (propertize (char-to-string first) 'face 'corfu-quick1)
                     (propertize (char-to-string second) 'face 'corfu-quick2))
             (cons first (list first))))))
      (let ((first (elt corfu-quick1 (mod idx fst))))
        (if two
            (list "  ")
          (list
           (concat (propertize (char-to-string first) 'face 'corfu-quick1) " ")
           (cons first (+ corfu--scroll idx))))))))

(defun corfu-quick--read (&optional first)
  "Read quick key given FIRST pressed key."
  (cl-letf* ((list nil)
             (space1 (propertize " " 'display
                                 `(space :width
                                         (+ 0.5 (,(alist-get
                                                   'child-frame-border-width
                                                   corfu--frame-parameters))))))
             (space2 #(" " 0 1 (display (space :width 0.5))))
             (orig (symbol-function #'corfu--affixate))
             ((symbol-function #'corfu--affixate)
              (lambda (cands)
                (setq cands (cdr (funcall orig cands)))
                (cl-loop for cand in cands for index from 0 do
                         (pcase-let ((`(,keys . ,events) (corfu-quick--keys first index)))
                           (setq list (nconc events list))
                           (setf (cadr cand) (concat space1 (propertize " " 'display keys) space2))))
                (cons t cands)))
             ;; Increase minimum width to avoid odd jumping
             (corfu-min-width (+ 3 corfu-min-width)))
    (corfu--candidates-popup
     (posn-at-point (+ (car completion-in-region--data) (length corfu--base))))
    (alist-get (read-key) list)))

;;;###autoload
(defun corfu-quick-jump ()
  "Jump to candidate using quick keys."
  (interactive)
  (when (fboundp 'corfu-echo--cancel)
    (corfu-echo--cancel))
  (if (= corfu--total 0)
      (and (message "No match") nil)
    (let ((idx (corfu-quick--read)))
      (when (consp idx) (setq idx (corfu-quick--read (car idx))))
      (when idx (setq corfu--index idx)))))

;;;###autoload
(defun corfu-quick-insert ()
  "Insert candidate using quick keys."
  (interactive)
  (when (corfu-quick-jump)
    (corfu-insert)))

;;;###autoload
(defun corfu-quick-complete ()
  "Complete candidate using quick keys."
  (interactive)
  (when (corfu-quick-jump)
    (corfu-complete)))

;; Emacs 28: Do not show Corfu commands in M-X
(dolist (sym '(corfu-quick-jump corfu-quick-insert corfu-quick-complete))
  (put sym 'completion-predicate #'ignore))

(provide 'corfu-quick)
;;; corfu-quick.el ends here
