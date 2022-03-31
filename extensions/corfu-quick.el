;;; corfu-quick.el --- Completion Overlay Region FUnction -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Luis Henriquez-Perez <luis@luishp.xyz>, Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (corfu "0.21"))
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

;; This package is a Corfu extension, which prefixes candidates with quick
;; keys. The term "quick keys" refers to letters displayed in the `corfu' popup
;; in front each candidate. Typing these quick keys allows you to select the
;; candidate in front of them. This is designed to be a faster alternative to
;; selecting a candidate with `corfu-next' and `corfu-previous'.

;;; Code:

(require 'corfu)

(defcustom corfu-quick1 "asdfgh"
  "First level quick keys."
  :type 'string
  :group 'corfu)

(defcustom corfu-quick2 "jkl"
  "Second level quick keys."
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

(defvar-local corfu-quick--alist nil
  "An alist whose elements are (QUICK-KEYS . INDEX).
QUICK-KEYS is a string containing the quick keys. INDEX is the index of
the candidate that corresponds to QUICK-KEYS.")

(defun corfu-quick--keys (index)
  "Return `corfu-quick' keys for candidate at INDEX."
  (let ((length1 (seq-length corfu-quick1))
        (length2 (seq-length corfu-quick2))
        (key1 "")
        (key2 ""))
    (if (< index length1)
        (setq key1 (char-to-string (seq-elt corfu-quick1 index)))
      (setq key1 (char-to-string (seq-elt corfu-quick1 (% (- index length1) length1))))
      (setq key2 (char-to-string (seq-elt corfu-quick2 (% (- index length1) length2)))))
    (concat (propertize key2 'face 'corfu-quick1)
            (propertize key1 'face 'corfu-quick2))))

(defun corfu-quick--format-candidates (orig candidates)
  "Advice for `corfu--format-candidates' that adds quick keys to candidates.
See `corfu--format-candidates'."
  (let ((updated-candidates nil)
        (quick-keys nil)
        (index 0))
    (setq corfu-quick--alist nil)
    (pcase-dolist (`(,candidate ,prefix ,suffix) candidates)
      (setq quick-keys (corfu-quick--keys index))
      (push (list candidate (concat quick-keys " " prefix) suffix) updated-candidates)
      (push (cons (substring-no-properties quick-keys) index) corfu-quick--alist)
      (cl-incf index))
    (setq updated-candidates (reverse updated-candidates))
    (funcall orig updated-candidates)))

(defun corfu-quick--read ()
  "Read quick keys and return index of candidate specified by quick keys."
  (cl-letf* ((old-fn (symbol-function #'corfu--format-candidates))
             (new-fn (apply-partially #'corfu-quick--format-candidates old-fn))
             ((symbol-function #'corfu--format-candidates) new-fn))
    (corfu--candidates-popup (point))
    (let* ((key (read-key))
           (quick-keys (char-to-string key)))
      (when (seq-contains-p corfu-quick2 key)
        (cl-letf* ((orig-fn (symbol-function #'corfu-quick--keys))
                   ((symbol-function #'corfu-quick--keys) (lambda (index) (seq-rest (funcall orig-fn index)))))
          (corfu--candidates-popup (point)))
        (setq quick-keys (char-to-string (read-key))))
      (or (alist-get quick-keys corfu-quick--alist 0 nil #'string=)
          (corfu-quick-exit)))))

;;;###autoload
(defun corfu-quick-jump ()
  "Jump to candidate using quick keys."
  (interactive)
  (if (zerop corfu--total)
      (and (message "No match") nil)
    (setq corfu--index (or (corfu-quick--read) corfu--index))))

;;;###autoload
(defun corfu-quick-insert ()
  "Insert candidate using quick keys."
  (interactive)
  (when (corfu-quick-jump)
    (corfu-insert)))

(provide 'corfu-quick)
;;; corfu-quick.el ends here
