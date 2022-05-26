;;; corfu-kind-grouping.el --- Sorting by king grouping for Corfu -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (corfu "0.25"))
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

;; Add a command `corfu-kind-grouping-toggle' to simulate narrow/widden.

;;; Code:

(require 'corfu)
(eval-when-compile (require 'cl-lib))

(defvar-local corfu-kind-grouping-enable nil
  "Sort grouping by kind or not.")

(defun corfu-kind-grouping--sort-advice (fn candicates)
  "Sort candicates, then resort according `corfu-kind-grouping-enable'."
  (let* ((ordered (funcall fn candicates))
         (kindfn (plist-get corfu--extra :company-kind)))
    (when (and corfu-kind-grouping-enable (functionp kindfn))
      (let ((kinds (delete-dups (mapcar (lambda (c) (funcall kindfn c)) ordered))))
        (when (> (length kinds) 1)
          (setq ordered
                (cl-loop for kind in kinds
                         append (cl-remove-if-not
                                 (lambda (c) (equal (funcall kindfn c) kind))
                                 ordered))))))
    ordered))

;;;###autoload
(defun corfu-kind-grouping-toggle ()
  "Redisplay candicates according `corfu-kind-grouping-enable'.
If `corfu-kind-grouping-enable' is true, then force display candicates
grouping by kind, else restore the order."
  (interactive)
  (setq corfu-kind-grouping-enable (not corfu-kind-grouping-enable))
  (let ((corfu--input nil)
        (cand (nth corfu--index corfu--candidates)))
    (corfu--update)
    (setq corfu--index (cl-position cand corfu--candidates :test #'string=))))

(advice-add #'corfu--sort-candicates :around #'corfu-kind-grouping--sort-advice)

(define-key corfu-map "\M-r" #'corfu-kind-grouping-toggle)

(provide 'corfu-kind-grouping)

;;; corfu-kind-grouping.el ends here
