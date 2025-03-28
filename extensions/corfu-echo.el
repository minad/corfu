;;; corfu-echo.el --- Show candidate documentation in echo area -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 2.0
;; Package-Requires: ((emacs "28.1") (compat "30") (corfu "2.0"))
;; URL: https://github.com/minad/corfu

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

;; Show candidate documentation in echo area.  Enable `corfu-echo-mode'.

;;; Code:

(require 'corfu)
(eval-when-compile
  (require 'subr-x))

(defface corfu-echo
  '((t :inherit completions-annotations))
  "Face used for echo area messages."
  :group 'corfu-faces)

(defcustom corfu-echo-delay '(2.0 . 1.0)
  "Show documentation string in the echo area after that number of seconds.
The value can be a pair of two floats to specify initial and
subsequent delay."
  :type '(choice (const :tag "Never" nil)
                 (number :tag "Delay in seconds")
                 (cons :tag "Two Delays"
                       (choice :tag "Initial   " number)
                       (choice :tag "Subsequent" number)))
  :group 'corfu)

(defvar corfu-echo--timer nil
  "Echo area message timer.")

(defvar corfu-echo--message nil
  "Last echo message.")

(defun corfu-echo--cancel (&optional msg)
  "Cancel echo timer and refresh MSG."
  (when corfu-echo--timer
    (cancel-timer corfu-echo--timer)
    (setq corfu-echo--timer nil))
  (corfu-echo--show msg)
  (unless corfu-echo--message
    (setq corfu-echo--timer nil
          corfu-echo--message nil)))

(defun corfu-echo--show (msg)
  "Show MSG in echo area."
  (when (or msg corfu-echo--message)
    (setq msg (or msg "")
          corfu-echo--message msg)
    (corfu--message "%s" (if (text-property-not-all 0 (length msg) 'face nil msg)
                             msg
                           (propertize msg 'face 'corfu-echo)))))

;;;###autoload
(define-minor-mode corfu-echo-mode
  "Show candidate documentation in echo area."
  :global t :group 'corfu)

(cl-defmethod corfu--exhibit :after (&context (corfu-echo-mode (eql t)) &optional _auto)
  (if-let (((not (minibufferp)))
           (delay (if (consp corfu-echo-delay)
                      (funcall (if corfu-echo--message #'cdr #'car)
                               corfu-echo-delay)
                    corfu-echo-delay))
           (fun (corfu--metadata-get 'company-docsig))
           (cand (and (>= corfu--index 0)
                      (nth corfu--index corfu--candidates))))
      (if (<= delay 0)
          (corfu-echo--show (funcall fun cand))
        (corfu-echo--cancel)
        (setq corfu-echo--timer
              (run-at-time delay nil
                           (lambda ()
                             (corfu-echo--show (funcall fun cand))))))
    (corfu-echo--cancel)))

(cl-defmethod corfu--teardown :before (_buf &context (corfu-echo-mode (eql t)))
  (corfu-echo--cancel))

(cl-defmethod corfu--prepare :before (&context (corfu-echo-mode (eql t)))
  ;; The refreshing is needed to prevent flicker if corfu-echo-delay=t.
  (corfu-echo--cancel corfu-echo--message))

(provide 'corfu-echo)
;;; corfu-echo.el ends here
