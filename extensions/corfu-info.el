;;; corfu-info.el --- Show candidate information in separate buffer -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
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

;; This Corfu extension provides commands to show additional information to the
;; candidates in a separate buffer. The commands `corfu-info-location' and
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

;;;###autoload
(defun corfu-info-documentation ()
  "Show documentation of current candidate."
  (interactive)
  ;; Company support, taken from `company.el', see `company-show-doc-buffer'.
  (when (< corfu--index 0)
    (user-error "No candidate selected"))
  (let ((cand (nth corfu--index corfu--candidates)))
    (if-let* ((fun (plist-get corfu--extra :company-doc-buffer))
              (res (funcall fun cand)))
        (let ((buf (or (car-safe res) res)))
          (corfu-info--restore-on-next-command)
          (setq other-window-scroll-buffer (get-buffer buf))
          (set-window-start (display-buffer buf t) (or (cdr-safe res) (point-min))))
      (user-error "No documentation available for `%s'" cand))))

;;;###autoload
(defun corfu-info-location ()
  "Show location of current candidate."
  (interactive)
  ;; Company support, taken from `company.el', see `company-show-location'.
  (when (< corfu--index 0)
    (user-error "No candidate selected"))
  (let ((cand (nth corfu--index corfu--candidates)))
    ;; BUG: company-location may throw errors if location is not found
    (if-let* ((fun (ignore-errors (plist-get corfu--extra :company-location)))
              (loc (funcall fun cand)))
        (let ((buf (or (and (bufferp (car loc)) (car loc))
                       (find-file-noselect (car loc) t))))
          (corfu-info--restore-on-next-command)
          (setq other-window-scroll-buffer buf)
          (with-selected-window (display-buffer buf t)
            (save-restriction
              (widen)
              (goto-char (point-min))
              (when-let (pos (cdr loc))
                (if (bufferp (car loc))
                    (goto-char pos)
                  (forward-line (1- pos))))
              (set-window-start nil (point)))))
      (user-error "No location available for `%s'" cand))))

;; Emacs 28: Do not show Corfu commands with M-X
(put #'corfu-info-location 'completion-predicate #'ignore)
(put #'corfu-info-documentation 'completion-predicate #'ignore)

(provide 'corfu-info)
;;; corfu-info.el ends here
