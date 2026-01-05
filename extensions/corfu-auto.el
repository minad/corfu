;;; corfu-auto.el --- Auto completion -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 2.7
;; Package-Requires: ((emacs "29.1") (compat "30") (corfu "2.7"))
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

;; Automatically show the popup.  Enable `corfu-auto-mode'.

;;; Code:

(require 'corfu)

(defcustom corfu-auto-trigger ""
  "Characters which trigger auto completion.
If a trigger character is detected `corfu-auto-prefix' is ignored."
  :type 'string
  :group 'corfu)

(defcustom corfu-auto-prefix 3
  "Minimum length of prefix for auto completion.
The completion backend can override this with :company-prefix-length.
It is not recommended to use a small prefix length (below 2), since this
will create high load for Emacs.  See also `corfu-auto-delay' and
`corfu-auto-trigger'."
  :type 'natnum
  :group 'corfu)

(defcustom corfu-auto-delay 0.2
  "Delay for auto completion.
It is not recommended to use a short delay or even 0, since this will
create high load for Emacs, in particular if executing the completion
backend is costly.  Instead of reducing the delay too much, try
`corfu-auto-trigger' to trigger immediate completion after certain
characters."
  :type 'float
  :group 'corfu)

(defcustom corfu-auto-commands
  '("self-insert-command\\'" "delete-backward-char\\'" "\\`backward-delete-char"
    c-electric-colon c-electric-lt-gt c-electric-slash c-scope-operator)
  "Commands which initiate auto completion.
The list can contain either command symbols or regular expressions."
  :type '(repeat (choice regexp symbol))
  :group 'corfu)

(defvar corfu-auto--timer (timer-create)
  "Auto completion timer.")

(defun corfu-auto--complete-deferred (&optional tick)
  "Initiate auto completion if TICK did not change."
  (corfu--protect
   (lambda ()
     (when (and (not completion-in-region-mode)
                (or (not tick) (equal tick (corfu-auto--tick))))
       (pcase (while-no-input ;; Interruptible Capf query
                (run-hook-wrapped
                 'completion-at-point-functions
                 #'corfu--capf-wrapper corfu-auto-prefix corfu-auto-trigger))
         (`(,fun ,beg ,end ,table . ,plist)
          (let ((completion-in-region-mode-predicate
                 (lambda ()
                   (when-let* ((newbeg (car-safe (funcall fun))))
                     (= newbeg beg))))
                (completion-extra-properties plist))
            (corfu--setup beg end table (plist-get plist :predicate))
            (corfu--exhibit 'auto))))))))

(defun corfu-auto--post-command ()
  "Post command hook which initiates auto completion."
  (corfu--protect
   (lambda ()
     (cancel-timer corfu-auto--timer)
     (when (and (not completion-in-region-mode)
                (not defining-kbd-macro)
                (not buffer-read-only)
                (corfu--match-symbol-p corfu-auto-commands this-command)
                (corfu--popup-support-p))
       (if (or (<= corfu-auto-delay 0)
               (seq-contains-p corfu-auto-trigger last-command-event))
           (corfu-auto--complete-deferred)
         ;; Do not use `timer-set-idle-time' since this leads to
         ;; unpredictable pauses, in particular with `flyspell-mode'.
         (timer-set-time corfu-auto--timer
                         (timer-relative-time nil corfu-auto-delay))
         (timer-set-function corfu-auto--timer #'corfu-auto--complete-deferred
                             (list (corfu-auto--tick)))
         (timer-activate corfu-auto--timer))))))

(defun corfu-auto--tick ()
  "Return the current tick/status of the buffer.
Auto completion is only performed if the tick did not change."
  (list (selected-window) (current-buffer) (buffer-chars-modified-tick) (point)))

(provide 'corfu-auto)
;;; corfu-auto.el ends here
