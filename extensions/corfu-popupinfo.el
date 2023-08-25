;;; corfu-popupinfo.el --- Candidate information popup for Corfu -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Yuwei Tian <fishtai0@gmail.com>, Daniel Mendler <mail@daniel-mendler.de>
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

;; Display an information popup for completion candidate when using
;; Corfu.  The popup displays either the candidate documentation or the
;; candidate location.  The `corfu-popupinfo-mode' must be enabled
;; globally.  Set `corfu-popupinfo-delay' to nil if the info popup should
;; not update automatically.  If the popup should not appear initially,
;; but update automatically afterwards, use `(setq corfu-popupinfo-delay
;; (cons nil 1.0))'.

;; For manual toggling the commands `corfu-popupinfo-toggle',
;; `corfu-popupinfo-location' and `corfu-popupinfo-documentation' are
;; bound in the `corfu-popupinfo-map'.

;;; Code:

(require 'corfu)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defface corfu-popupinfo
  '((t :inherit corfu-default))
  "Face used for the info popup."
  :group 'corfu-faces)

(defcustom corfu-popupinfo-delay '(2.0 . 1.0)
  "Automatically update info popup after that number of seconds.

The value can be a pair of two floats to specify initial and
subsequent delay.  If the value is non-nil or the car of the pair
is non-nil, the popup will automatically appear for the
preselected candidate.  Otherwise the popup can be requested
manually via `corfu-popupinfo-toggle',
`corfu-popupinfo-documentation' and `corfu-popupinfo-location'.

It is *not recommended* to use a short delay or even 0, since
this will create high load for Emacs.  Retrieving the
documentation from the backend is usually expensive."
  :type '(choice (const :tag "Never" nil)
                 (number :tag "Delay in seconds")
                 (cons :tag "Two Delays"
                       (choice :tag "Initial   "
                               (choice (const nil) number))
                       (choice :tag "Subsequent"
                               (choice (const nil) number))))
  :group 'corfu)

(defcustom corfu-popupinfo-hide t
  "Hide the popup during the transition between candidates."
  :type 'boolean
  :group 'corfu)

(defcustom corfu-popupinfo-max-width 80
  "The maximum width of the info popup in characters."
  :type 'natnum
  :group 'corfu)

(defcustom corfu-popupinfo-min-width 30
  "The minimum width of the info popup in characters."
  :type 'natnum
  :group 'corfu)

(defcustom corfu-popupinfo-max-height 10
  "The maximum height of the info popup in characters."
  :type 'natnum
  :group 'corfu)

(defcustom corfu-popupinfo-min-height 1
  "The minimum height of the info popup in characters."
  :type 'natnum
  :group 'corfu)

(defcustom corfu-popupinfo-resize t
  "Resize the info popup automatically if non-nil."
  :type 'boolean
  :group 'corfu)

(defcustom corfu-popupinfo-direction '(right left vertical)
  "Preferred directions for the popup in order."
  :type '(repeat
          (choice
           (const left)
           (const right)
           (const vertical)
           (const force-left)
           (const force-right)
           (const force-horizontal)
           (const force-vertical)))
  :group 'corfu)

(defvar-keymap corfu-popupinfo-map
  :doc "Additional keymap activated in popupinfo mode."
  "M-t" #'corfu-popupinfo-toggle
  "<remap> <corfu-info-documentation>" #'corfu-popupinfo-documentation
  "<remap> <corfu-info-location>" #'corfu-popupinfo-location
  "<remap> <scroll-other-window>" #'corfu-popupinfo-scroll-up
  "<remap> <scroll-other-window-down>" #'corfu-popupinfo-scroll-down
  "<remap> <end-of-buffer-other-window>" #'corfu-popupinfo-end
  "<remap> <beginning-of-buffer-other-window>" #'corfu-popupinfo-beginning)

(defvar corfu-popupinfo--buffer-parameters
  '((truncate-partial-width-windows . nil)
    (truncate-lines . nil)
    (left-margin-width . 1)
    (right-margin-width . 1)
    (word-wrap . t)
    (fringe-indicator-alist (continuation)))
  "Buffer parameters.")

(defvar-local corfu-popupinfo--toggle 'init
  "Local toggle state.")

(defvar-local corfu-popupinfo--function
  #'corfu-popupinfo--get-documentation
  "Function called to obtain documentation string.")

(defvar corfu-popupinfo--frame nil
  "Info popup child frame.")

(defvar corfu-popupinfo--timer nil
  "Corfu info popup auto display timer.")

(defvar-local corfu-popupinfo--candidate nil
  "Completion candidate for the info popup.")

(defvar-local corfu-popupinfo--coordinates nil
  "Coordinates of the candidate popup.
The coordinates list has the form (LEFT TOP RIGHT BOTTOM) where
all values are in pixels relative to the origin.  See
`frame-edges' for details.")

(defvar-local corfu-popupinfo--lock-dir nil
  "Locked position direction of the info popup.")

(defconst corfu-popupinfo--state-vars
  '(corfu-popupinfo--candidate
    corfu-popupinfo--coordinates
    corfu-popupinfo--lock-dir
    corfu-popupinfo--toggle
    corfu-popupinfo--function)
  "Buffer-local state variables used by corfu-popupinfo.")

(defun corfu-popupinfo--visible-p (&optional frame)
  "Return non-nil if FRAME is visible."
  (setq frame (or frame corfu-popupinfo--frame))
  (and (frame-live-p frame) (frame-visible-p frame)))

(defun corfu-popupinfo--get-location (candidate)
  "Get source at location of CANDIDATE."
  (save-excursion
    (let ((old-buffers (buffer-list)) (buffer nil))
      (unwind-protect
          (when-let
              ((fun (plist-get corfu--extra :company-location))
               ;; BUG: company-location may throw errors if location is not found
               (loc (ignore-errors (funcall fun candidate)))
               ((setq buffer
                      (or (and (bufferp (car loc)) (car loc))
                          (get-file-buffer (car loc))
                          (let ((inhibit-message t)
                                (message-log-max nil)
                                (inhibit-redisplay t)
                                (enable-dir-local-variables nil)
                                (enable-local-variables :safe)
                                (non-essential t)
                                (delay-mode-hooks t)
                                (find-file-hook '(global-font-lock-mode-check-buffers)))
                            (find-file-noselect (car loc) t))))))
            (with-current-buffer buffer
              (save-excursion
                (without-restriction
                  (goto-char (point-min))
                  (when-let (pos (cdr loc))
                    (if (bufferp (car loc))
                        (goto-char pos)
                      (forward-line (1- pos))))
                  (let ((beg (point)))
                    ;; Support a little bit of scrolling.
                    (forward-line (* 10 corfu-popupinfo-max-height))
                    (when jit-lock-mode
                      (jit-lock-fontify-now beg (point)))
                    (let ((res (buffer-substring beg (point))))
                      (and (not (string-blank-p res)) res)))))))
        (when (and buffer (not (memq buffer old-buffers)))
          (kill-buffer buffer))))))

(defun corfu-popupinfo--get-documentation (candidate)
  "Get the documentation for CANDIDATE."
  (when-let ((fun (plist-get corfu--extra :company-doc-buffer))
             (res (save-excursion
                    (let ((inhibit-message t)
                          (message-log-max nil)
                          (inhibit-redisplay t)
                          ;; Reduce print length for elisp backend (#249)
                          (print-level 3)
                          (print-length (* corfu-popupinfo-max-width
                                           corfu-popupinfo-max-height)))
                      (funcall fun candidate)))))
    (with-current-buffer (or (car-safe res) res)
      (setq res (string-trim
                 (replace-regexp-in-string
                  "[\n\t ]*\\[back\\][\n\t ]*" ""
                  (buffer-string))))
      (and (not (string-blank-p res)) res))))

(defun corfu-popupinfo--size ()
  "Return popup size as pair."
  (let* ((cw (default-font-width))
         (lh (default-line-height))
         (margin
          (* cw (+ (alist-get 'left-margin-width corfu-popupinfo--buffer-parameters)
                   (alist-get 'right-margin-width corfu-popupinfo--buffer-parameters))))
         (max-height (* lh corfu-popupinfo-max-height))
         (max-width (* cw corfu-popupinfo-max-width)))
    (or (when corfu-popupinfo-resize
          (with-current-buffer " *corfu-popupinfo*"
            (cl-letf* (((window-dedicated-p) nil)
                       ((window-buffer) (current-buffer))
                       (size (window-text-pixel-size
                              nil (point-min) (point-max)
                              ;; Use 3*max-height as y-limit, to take more text
                              ;; into account.
                              max-width (* 3 max-height))))
              ;; Check that width is not exceeded. Otherwise use full height,
              ;; since lines will get wrapped.
              (when (<= (car size) max-width)
                (cons (+ margin (car size))
                      ;; XXX HACK: Ensure that popup has at least a height of 1,
                      ;; which is the minimum frame height (#261). Maybe we
                      ;; should ask upstream how smaller frames can be created.
                      ;; I only managed to create smaller frames by setting
                      ;; `window-safe-min-height' to 0, which feels problematic.
                      (min (max (cdr size) lh) max-height))))))
        (cons (+ margin max-width) max-height))))

(defun corfu-popupinfo--frame-geometry (frame)
  "Return position and size geometric attributes of FRAME.

The geometry represents the position and size in pixels
in the form of (X Y WIDTH HEIGHT)."
  (pcase-let ((`(,x . ,y) (frame-position frame)))
    (list x y (frame-pixel-width frame) (frame-pixel-height frame))))

(defun corfu-popupinfo--fits-p (size area)
  "Check if SIZE fits into the AREA.

SIZE is in the form (WIDTH . HEIGHT).
AREA is in the form (X Y WIDTH HEIGHT DIR)."
  (and (>= (nth 2 area) (car size)) (>= (nth 3 area) (cdr size))))

(defun corfu-popupinfo--larger-p (area1 area2)
  "Check if AREA1 is larger than AREA2.

AREA1 and AREA2 are both in the form (X Y WIDTH HEIGHT DIR)."
  (>= (* (nth 2 area1) (nth 3 area1)) (* (nth 2 area2) (nth 3 area2))))

(defun corfu-popupinfo--area (ps)
  "Calculate the display area for the info popup.

PS is the pixel size of the popup.  The calculated area is in the
form (X Y WIDTH HEIGHT DIR)."
  (pcase-let*
      ((cw (default-font-width))
       (lh (default-line-height))
       (border (alist-get 'child-frame-border-width corfu--frame-parameters))
       (`(,_pfx ,_pfy ,pfw ,pfh)
        (corfu-popupinfo--frame-geometry (frame-parent corfu--frame)))
       (`(,cfx ,cfy ,cfw ,cfh) (corfu-popupinfo--frame-geometry corfu--frame))
       ;; Candidates popup below input
       (below (>= cfy (+ lh (cadr (window-inside-pixel-edges))
                         (window-tab-line-height)
                         (or (cdr (posn-x-y (posn-at-point (point)))) 0))))
       ;; Popups aligned at top
       (top-aligned (or below (< (cdr ps) cfh)))
       ;; Left display area
       (ahy (if top-aligned
                cfy
              (max 0 (- (+ cfy cfh) border border (cdr ps)))))
       (ahh (if top-aligned
                (min (- pfh cfy) (cdr ps))
              (min (- (+ cfy cfh) border border) (cdr ps))))
       (al (list (max 0 (- cfx (car ps) border)) ahy
                 (min (- cfx border) (car ps)) ahh 'left))
       ;; Right display area
       (arx (+ cfx cfw (- border)))
       (ar (list arx ahy (min (- pfw arx border border) (car ps)) ahh 'right))
       ;; Vertical display area
       (avw (min (car ps) (- pfw cfx border border)))
       (av (if below
               (list cfx (+ cfy cfh (- border)) avw (min (- pfh cfy cfh border) (cdr ps)) 'vertical)
             (let ((h (min (- cfy border border) (cdr ps))))
               (list cfx (max 0 (- cfy h border)) avw h 'vertical)))))
    (unless (and corfu-popupinfo--lock-dir
                 (corfu-popupinfo--fits-p
                  (cons (* cw corfu-popupinfo-min-width) (* lh corfu-popupinfo-min-height))
                  (pcase corfu-popupinfo--lock-dir ('left al) ('right ar) ('vertical av))))
      (setq corfu-popupinfo--lock-dir nil))
    (or
     (cl-loop for dir in corfu-popupinfo-direction thereis
              (pcase dir
                ((or 'force-right (guard (eq corfu-popupinfo--lock-dir 'right))) ar)
                ((or 'force-left (guard (eq corfu-popupinfo--lock-dir 'left))) al)
                ((or 'force-vertical (guard (eq corfu-popupinfo--lock-dir 'vertical))) av)
                ((and 'right (guard (corfu-popupinfo--fits-p ps ar))) ar)
                ((and 'left (guard (corfu-popupinfo--fits-p ps al))) al)
                ((and 'vertical (guard (corfu-popupinfo--fits-p ps av))) av)))
     (let ((ah (if (corfu-popupinfo--larger-p ar al) ar al)))
       (if (corfu-popupinfo--larger-p av ah) av ah)))))

(defun corfu-popupinfo--show (candidate)
  "Show the info popup for CANDIDATE."
  (when corfu-popupinfo--timer
    (cancel-timer corfu-popupinfo--timer)
    (setq corfu-popupinfo--timer nil))
  (when (and (corfu-popupinfo--visible-p corfu--frame))
    (let* ((cand-changed
            (not (and (corfu-popupinfo--visible-p)
                      (equal candidate corfu-popupinfo--candidate))))
           (new-coords (frame-edges corfu--frame 'inner-edges))
           (coords-changed (not (equal new-coords corfu-popupinfo--coordinates))))
      (when cand-changed
        (if-let (content (funcall corfu-popupinfo--function candidate))
            (with-current-buffer (corfu--make-buffer " *corfu-popupinfo*")
              (with-silent-modifications
                (erase-buffer)
                (insert content)
                (goto-char (point-min)))
              (dolist (var corfu-popupinfo--buffer-parameters)
                (set (make-local-variable (car var)) (cdr var)))
              (when-let ((m (memq 'corfu-default (alist-get 'default face-remapping-alist))))
                (setcar m 'corfu-popupinfo)))
          (corfu-popupinfo--hide)
          (setq cand-changed nil coords-changed nil)))
      (when (or cand-changed coords-changed)
        (pcase-let* ((border (alist-get 'child-frame-border-width corfu--frame-parameters))
                     (`(,area-x ,area-y ,area-w ,area-h ,area-d)
                      (corfu-popupinfo--area
                       (if cand-changed
                           (corfu-popupinfo--size)
                         (cons
                          (- (frame-pixel-width corfu-popupinfo--frame) border border)
                          (- (frame-pixel-height corfu-popupinfo--frame) border border)))))
                     (margin-quirk (not corfu-popupinfo--frame)))
          (setq corfu-popupinfo--frame
                (corfu--make-frame corfu-popupinfo--frame
                                   area-x area-y area-w area-h
                                   " *corfu-popupinfo*")
                corfu-popupinfo--toggle t
                corfu-popupinfo--lock-dir area-d
                corfu-popupinfo--candidate candidate
                corfu-popupinfo--coordinates new-coords)
          ;; XXX HACK: Force margin update. For some reason, the call to
          ;; `set-window-buffer' in `corfu--make-frame' is not effective the
          ;; first time. Why does Emacs have all these quirks?
          (when margin-quirk
            (set-window-buffer
             (frame-root-window corfu-popupinfo--frame)
             " *corfu-popupinfo*")))))))

(defun corfu-popupinfo--hide ()
  "Clear the info popup buffer content and hide it."
  (corfu--hide-frame corfu-popupinfo--frame))

(defun corfu-popupinfo-end (&optional n)
  "Scroll text of info popup window to its end.

If arg N is omitted or nil, scroll to end.  If a numerical value,
put point N/10 of the way from the end.  If the info popup is not
visible, the other window is moved to beginning or end."
  (interactive "P")
  (if (corfu-popupinfo--visible-p)
      (with-selected-frame corfu-popupinfo--frame
        (with-current-buffer " *corfu-popupinfo*"
          (with-no-warnings
            (end-of-buffer n))))
    (end-of-buffer-other-window n)))

(defun corfu-popupinfo-beginning (&optional n)
  "Scroll text of info popup window to beginning of buffer.

See `corfu-popupinfo-end' for the argument N."
  (interactive "P")
  (corfu-popupinfo-end (- 10 (if (numberp n) n 0))))

(defun corfu-popupinfo-scroll-up (&optional n)
  "Scroll text of info popup window upward N lines.

If ARG is omitted or nil, scroll upward by a near full screen.
See `scroll-up' for details.  If the info popup is not visible,
the other window is scrolled."
  (interactive "p")
  (if (corfu-popupinfo--visible-p)
      (with-selected-frame corfu-popupinfo--frame
        (with-current-buffer " *corfu-popupinfo*"
          (scroll-up n)))
    (scroll-other-window n)))

(defun corfu-popupinfo-scroll-down (&optional n)
  "Scroll text of info popup window down N lines.

See `corfu-popupinfo-scroll-up' for more details."
  (interactive "p")
  (corfu-popupinfo-scroll-up (- (or n 1))))

(defun corfu-popupinfo--toggle (fun)
  "Set documentation getter FUN and toggle popup."
  (when (< corfu--index 0)
    (corfu-popupinfo--hide)
    (user-error "No candidate selected"))
  (setq corfu-popupinfo--toggle
        (not (and (corfu-popupinfo--visible-p)
                  (eq corfu-popupinfo--function fun))))
  (if (not corfu-popupinfo--toggle)
      (corfu-popupinfo--hide)
    (setq corfu-popupinfo--function fun
          corfu-popupinfo--candidate nil)
    (let ((cand (nth corfu--index corfu--candidates)))
      (corfu-popupinfo--show cand)
      (unless (corfu-popupinfo--visible-p)
        (user-error "No %s available for `%s'"
                    (car (last (split-string (symbol-name fun) "-+")))
                    cand)))))

(defun corfu-popupinfo-documentation ()
  "Show or hide documentation in popup.
Behaves like `corfu-popupinfo-toggle'."
  (interactive)
  (corfu-popupinfo--toggle #'corfu-popupinfo--get-documentation))

(defun corfu-popupinfo-location ()
  "Show or hide location in popup.
Behaves like `corfu-popupinfo-toggle'."
  (interactive)
  (corfu-popupinfo--toggle #'corfu-popupinfo--get-location))

(defun corfu-popupinfo-toggle ()
  "Toggle the info popup display or hide.

When using this command to manually hide the info popup, it will
not be displayed until this command is called again, even if
`corfu-popupinfo-delay' is non-nil."
  (interactive)
  (corfu-popupinfo--toggle corfu-popupinfo--function))

;;;###autoload
(define-minor-mode corfu-popupinfo-mode
  "Corfu info popup minor mode."
  :global t :group 'corfu)

(cl-defmethod corfu--exhibit :after (&context (corfu-popupinfo-mode (eql t)) &optional _auto)
  (when completion-in-region-mode
    (setf (alist-get #'corfu-popupinfo-mode minor-mode-overriding-map-alist)
          corfu-popupinfo-map)
    (when corfu-popupinfo--timer
      (cancel-timer corfu-popupinfo--timer)
      (setq corfu-popupinfo--timer nil))
    (if (and (>= corfu--index 0) (corfu-popupinfo--visible-p corfu--frame))
        (let ((cand (nth corfu--index corfu--candidates)))
          (if-let ((delay (if (consp corfu-popupinfo-delay)
                              (funcall (if (eq corfu-popupinfo--toggle 'init) #'car #'cdr)
                                       corfu-popupinfo-delay)
                            corfu-popupinfo-delay))
                   (corfu-popupinfo--toggle))
              (if (or (<= delay 0)
                      (and (equal cand corfu-popupinfo--candidate)
                           (corfu-popupinfo--visible-p)))
                  (corfu-popupinfo--show cand)
                (when (corfu-popupinfo--visible-p)
                  (cond
                   (corfu-popupinfo-hide
                    (corfu-popupinfo--hide))
                   (corfu-popupinfo--candidate
                    (corfu-popupinfo--show corfu-popupinfo--candidate))))
                (setq corfu-popupinfo--timer
                    (run-at-time delay nil #'corfu-popupinfo--show cand)))
            (unless (equal cand corfu-popupinfo--candidate)
              (corfu-popupinfo--hide))))
      (corfu-popupinfo--hide))))

(cl-defmethod corfu--teardown :before (&context (corfu-popupinfo-mode (eql t)))
  (corfu-popupinfo--hide)
  (mapc #'kill-local-variable corfu-popupinfo--state-vars)
  (setq minor-mode-overriding-map-alist
        (assq-delete-all #'corfu-popupinfo-mode
                         minor-mode-overriding-map-alist)))

;; Emacs 28: Do not show Corfu commands with M-X
(dolist (sym '(corfu-popupinfo-scroll-down corfu-popupinfo-scroll-up
               corfu-popupinfo-documentation corfu-popupinfo-location
               corfu-popupinfo-beginning corfu-popupinfo-end
               corfu-popupinfo-toggle))
  (put sym 'completion-predicate #'ignore))

(provide 'corfu-popupinfo)
;;; corfu-popupinfo.el ends here
