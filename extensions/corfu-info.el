;;; corfu-info.el --- Show candidate information in separate buffer -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Free Software Foundation, Inc.

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

;; This Corfu extension provides commands to show additional information to the
;; candidates in a separate buffer.  The commands `corfu-info-location' and
;; `corfu-info-documentation' are bound by default in the `corfu-map' to M-g and
;; M-h respectively.

;;; Code:

(require 'corfu)
(eval-when-compile
  (require 'subr-x))

(defun corfu-info--restore-on-next-command ()
  "Restore window configuration before next command."
  (let ((config (current-window-configuration))
        (other other-window-scroll-buffer)
        (restore (make-symbol "corfu--restore")))
    (fset restore
          (lambda ()
            (setq other-window-scroll-buffer other)
            (unless (memq this-command '(scroll-other-window scroll-other-window-down))
              (when (memq this-command '(corfu-quit corfu-reset))
                (setq this-command #'ignore))
              (remove-hook 'pre-command-hook restore)
              (set-window-configuration config))))
    (add-hook 'pre-command-hook restore)))

(defun corfu-info--display-buffer (buffer name)
  "Display BUFFER and return window displaying the buffer.
Make the buffer persistent with NAME if non-nil."
  (if name
      (unless (buffer-local-value 'buffer-file-name buffer)
        (if-let ((old (get-buffer name)))
            (setq buffer (prog1 old (kill-buffer buffer)))
          (with-current-buffer buffer
            (rename-buffer name))))
    (corfu-info--restore-on-next-command))
  (setq other-window-scroll-buffer buffer)
  (display-buffer buffer t))

;;;###autoload
(defun corfu-info-documentation (&optional arg)
  "Show documentation of current candidate.
If called with a prefix ARG, the buffer is persistent."
  (interactive "P")
  ;; Company support, taken from `company.el', see `company-show-doc-buffer'.
  (when (< corfu--index 0)
    (user-error "No candidate selected"))
  (let ((cand (nth corfu--index corfu--candidates)))
    (if-let ((fun (corfu--metadata-get 'company-doc-buffer))
             (res (funcall fun cand)))
        (set-window-start (corfu-info--display-buffer
                           (get-buffer (or (car-safe res) res))
                           (and arg (format "*corfu doc: %s*" cand)))
                          (or (cdr-safe res) (point-min)))
      (user-error "No documentation available for `%s'" cand))))

;;;###autoload
(defun corfu-info-location (&optional arg)
  "Show location of current candidate.
If called with a prefix ARG, the buffer is persistent."
  (interactive "P")
  ;; Company support, taken from `company.el', see `company-show-location'.
  (when (< corfu--index 0)
    (user-error "No candidate selected"))
  (let ((cand (nth corfu--index corfu--candidates)))
    (if-let ((fun (corfu--metadata-get 'company-location))
             ;; BUG: company-location may throw errors if location is not found
             (loc (ignore-errors (funcall fun cand))))
        (with-selected-window
            (corfu-info--display-buffer
             (or (and (bufferp (car loc)) (car loc))
                 (find-file-noselect (car loc) t))
             (and arg (format "*corfu loc: %s*" cand)))
          (without-restriction
            (goto-char (point-min))
            (when-let ((pos (cdr loc)))
              (if (bufferp (car loc))
                  (goto-char pos)
                (forward-line (1- pos))))
            (set-window-start nil (point))))
      (user-error "No location available for `%s'" cand))))

(provide 'corfu-info)
;;; corfu-info.el ends here
