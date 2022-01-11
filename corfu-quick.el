;;; corfu-quick.el --- Completion Overlay Region FUnction -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.17
;; Package-Requires: ((emacs "27.1"))
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a corfu extension, which provides quick keys.

;;; Code:

;; Taken directly from `corfu'.

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defcustom corfu-quick1 "asdfgh"
  "Single level quick keys."
  :type 'string
  :group 'corfu)

(defcustom corfu-quick2 "jkl"
  "Two level quick keys."
  :type 'string
  :group 'corfu)

(defface corfu-quick1
  '((((class color) (min-colors 88) (background dark))
     :background "#7042a2" :weight bold :foreground "white")
    (((class color) (min-colors 88) (background light))
     :weight bold :background "#d5baff" :foreground "black")
    (t :background "magenta" :foreground "white"))
  "Face used for the first quick key."
  :group 'corfu-faces)

(defface corfu-quick2
  '((((class color) (min-colors 88) (background dark))
     :background "#004065" :weight bold :foreground "white")
    (((class color) (min-colors 88) (background light))
     :weight bold :background "#8ae4f2" :foreground "black")
    (t :background "blue" :foreground "white"))
  "Face used for the second quick key."
  :group 'corfu-faces)

(defvar-local corfu-quick--list nil
  "An alist whose elements are (quick-key . index).")
(defvar-local corfu-quick--first nil)

(defun corfu-quick--letters (index)
  "Return corfu quick letter for candidate at index."
  (let ((length1 (seq-length corfu-quick1))
	(length2 (seq-length corfu-quick2))
	(key1 "")
	(key2 ""))
    (if (< index length1)
	(setq key1 (char-to-string (seq-elt corfu-quick1 index)))
      (setq key1 (char-to-string (seq-elt corfu-quick1 (/ (- index length1) length1))))
      (setq key2 (char-to-string (seq-elt corfu-quick2 (% (- index length1) length2)))))
    (concat (propertize key1 'face 'corfu-quick1)
	    (propertize key2 'face 'corfu-quick2))))

(defun corfu-quick--format-candidates (orig candidates)
  "Format candidate, see `corfu--format-candidates' for arguments."
  (let ((updated-candidates nil)
	(index 0))
    (setq corfu-quick--list nil)
    (pcase-dolist (`(,candidate ,prefix ,suffix) candidates)
      (setq quick-letters (corfu-quick--letters index))
      (push (list candidate (concat quick-letters " " prefix) suffix) updated-candidates)
      (push (cons (substring-no-properties quick-letters) index) corfu-quick--list)
      (cl-incf index))
    (setq updated-candidates (reverse updated-candidates))
    (funcall orig updated-candidates)))

(defun corfu-quick--read ()
  "Read quick key given FIRST pressed key."
  (cl-letf* ((old-fn (symbol-function #'corfu--format-candidates))
	     (new-fn (apply-partially #'corfu-quick--format-candidates old-fn))
	     ((symbol-function #'corfu--format-candidates) new-fn))
    ;; Refresh the popup.
    (corfu--candidates-popup (point))
    (let ((first-char (char-to-string (read-string)))
	  (second-char (if corfu-quick2 (char-to-string (read-string)) "")))
      (alist-get (concat first-char second-char) corfu-quick--list 0 nil #'string=))))

;;;###autoload
(defun corfu-quick-exit ()
  (corfu--candidates-popup (point)))

;;;###autoload
(defun corfu-quick-jump ()
  "Jump to candidate using quick-keys."
  (interactive)
  (if (zerop corfu--total)
      (and (message "No match") nil)
    (setq corfu--index (corfu-quick--read))))

