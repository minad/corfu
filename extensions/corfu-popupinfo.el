;;; corfu-popupinfo.el --- Candidate information popup for Corfu -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

;; Author: Yuwei Tian <fishtai0@gmail.com>, Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (corfu "0.29"))
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

;; NOTE: This extension has been added recently to Corfu. It is still
;; experimental. The extension may get renamed and the public interface
;; may change any time.
;;
;; Display a documentation popup for completion candidate when using
;; Corfu. The `corfu-popupinfo-mode' must be enabled globally. Set
;; `corfu-popupinfo-delay' to nil if the documentation popup should not
;; appear automatically.

;; For manual toggling the commands `corfu-popupinfo-toggle',
;; `corfu-popupinfo-location' and `corfu-popupinfo-documentation' are
;; bound in the `corfu-popupinfo-map'.

;;; Code:

(require 'corfu)
(eval-when-compile
  (require 'subr-x))

(defface corfu-popupinfo
  '((t :inherit corfu-default :height 0.8))
  "Face used for the info popup."
  :group 'corfu-faces)

(defcustom corfu-popupinfo-delay '(1.0 . 0.5)
  "Show documentation popup after that number of seconds.
Set to t for an instant message. The value can be a pair of two
floats to specify initial and subsequent delay."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Instant" t)
                 (number :tag "Delay in seconds")
                 (cons :tag "Two Delays"
                       (choice :tag "Initial   " number)
                       (choice :tag "Subsequent" number)))
  :group 'corfu)

(defcustom corfu-popupinfo-hide t
  "Hide the popup during the transition between candidates."
  :group 'corfu
  :type 'boolean)

(defcustom corfu-popupinfo-max-width 70
  "The max width of the info popup in characters."
  :group 'corfu
  :type 'integer)

(defcustom corfu-popupinfo-max-height 10
  "The max height of the info popup in characters."
  :group 'corfu
  :type 'integer)

(defcustom corfu-popupinfo-resize t
  "Resize the info popup automatically if non-nil."
  :group 'corfu
  :type 'boolean)

(defvar corfu-popupinfo-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-d" #'corfu-popupinfo-documentation)
    (define-key map "\M-l" #'corfu-popupinfo-location)
    (define-key map "\M-t" #'corfu-popupinfo-toggle)
    (define-key map [remap scroll-other-window] #'corfu-popupinfo-scroll-up)
    (define-key map [remap scroll-other-window-down] #'corfu-popupinfo-scroll-down)
    map)
  "Additional keymap activated in popupinfo mode.")

(defvar corfu-popupinfo--buffer-parameters
  '((line-move-visual . t)
    (truncate-partial-width-windows . nil)
    (truncate-lines . nil)
    (left-margin-width . 1)
    (right-margin-width . 1)
    (word-wrap . t)
    (fringe-indicator-alist (continuation)))
  "Buffer parameters.")

(defvar-local corfu-popupinfo--toggle t
  "Local popupinfo toggle state.")

(defvar-local corfu-popupinfo--function
  #'corfu-popupinfo--get-documentation
  "Function called to obtain documentation string.")

(defvar corfu-popupinfo--frame nil
  "Info popup child frame.")

(defvar corfu-popupinfo--auto-timer nil
  "Corfu info popup auto display timer.")

(defvar-local corfu-popupinfo--candidate nil
  "Completion candidate for the info popup.")

(defvar-local corfu-popupinfo--coordinates nil
  "Coordinates of the candidate popup.
The coordinates list has the form (LEFT TOP RIGHT BOTTOM) where
all values are in pixels relative to the origin. See
`frame-edges' for details.")

(defvar-local corfu-popupinfo--direction nil
  "Position direction of the info popup relative to the candidate popup.")

(defconst corfu-popupinfo--state-vars
  '(corfu-popupinfo--candidate
    corfu-popupinfo--coordinates
    corfu-popupinfo--direction
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
    (when-let* ((fun (plist-get corfu--extra :company-location))
                (loc (funcall fun candidate))
                (res (or (and (bufferp (car loc)) (car loc))
                         (find-file-noselect (car loc) t))))
      (with-current-buffer res
        (save-excursion
          (save-restriction
            (widen)
            (if (bufferp (car loc))
                (goto-char (cdr loc))
              (goto-char (point-min))
              (forward-line (1- (cdr loc))))
            (let ((beg (point)))
              ;; Support a little bit of scrolling.
              (forward-line (* 10 corfu-popupinfo-max-height))
              (when jit-lock-mode
                (jit-lock-fontify-now beg (point)))
              (setq res (buffer-substring beg (point)))
              (and (not (string-blank-p res)) res))))))))

(defun corfu-popupinfo--get-documentation (candidate)
  "Get the documentation for CANDIDATE."
  (when-let* ((fun (plist-get corfu--extra :company-doc-buffer))
              (res (save-excursion
                     (let ((inhibit-message t)
                           (message-log-max nil)
                           ;; Reduce print length for elisp backend (#249)
                           (print-level 3)
                           (print-length (* corfu-popupinfo-max-width
                                            corfu-popupinfo-max-height)))
                       (funcall fun candidate)))))
    (with-current-buffer (or (car-safe res) res)
      (setq res (replace-regexp-in-string
                 "[\\s-\n]*\\[back\\][\\s-\n]*" ""
                 (buffer-string)))
      (and (not (string-blank-p res)) res))))

(defun corfu-popupinfo--size ()
  "Return popup size as pair."
  (let ((max-width (* (frame-char-width) corfu-popupinfo-max-width))
        (max-height (* (default-line-height) corfu-popupinfo-max-height)))
    (if corfu-popupinfo-resize
        (pcase-let ((`(,width . ,height)
                     (save-window-excursion
                       (with-current-buffer " *corfu-popupinfo*"
                         (set-window-dedicated-p nil nil)
                         (set-window-buffer nil (current-buffer))
                         (window-text-pixel-size nil (point-min) (point-max)
                                                 (* 2 max-width) (* 2 max-height))))))
          (cons (min width max-width) (min height max-height)))
      (cons max-width max-height))))

(defun corfu-popupinfo--frame-geometry (frame)
  "Return position and size geometric attributes of FRAME.

The geometry represents the position and size in pixels
in the form of (X Y WIDTH HEIGHT)."
  (pcase-let ((`(,x . ,y) (frame-position frame)))
    (list x y (frame-pixel-width frame) (frame-pixel-height frame))))

(defun corfu-popupinfo--display-area-horizontal (width height)
  "Calculate the horizontal display area for the info popup.

The WIDTH and HEIGHT of the info popup are in pixels.
The calculated area is in the form (X Y WIDTH HEIGHT DIR).
DIR indicates the horizontal position direction of the info popup
relative to the candidate popup, its value can be 'right or 'left."
  (pcase-let* ((border (alist-get 'child-frame-border-width corfu--frame-parameters))
               (`(,_pfx ,_pfy ,pfw ,_pfh)
                (corfu-popupinfo--frame-geometry (frame-parent corfu--frame)))
               (`(,cfx ,cfy ,cfw ,_cfh) (corfu-popupinfo--frame-geometry corfu--frame))
               (x-on-right (+ cfx cfw (- border)))
               (x-on-left (- cfx width border))
               (w-remaining-right (- pfw 1 x-on-right border border))
               (w-remaining-left (- cfx 1 border)))
    (cond
     ((>= w-remaining-right width)
      (list x-on-right cfy width height 'right))
     ((>= w-remaining-left width)
      (list x-on-left cfy width height 'left))
     ((>= w-remaining-right w-remaining-left)
      (list x-on-right cfy w-remaining-right height 'right))
     (t
      (list x-on-left cfy w-remaining-left height 'left)))))

(defun corfu-popupinfo--display-area-vertical (width height)
  "Calculate the vertical display area for the info popup.

The WIDTH and HEIGHT of the info popup are in pixels.

The calculated area is in the form (X Y WIDTH HEIGHT DIR).
DIR indicates the vertical position direction of the info popup
relative to the candidate popup, its value can be 'bottom or 'top."
  (pcase-let* ((border (alist-get 'child-frame-border-width corfu--frame-parameters))
               (lh (default-line-height))
               (`(,_pfx ,_pfy ,pfw ,pfh)
                (corfu-popupinfo--frame-geometry (frame-parent corfu--frame)))
               (`(,cfx ,cfy ,_cfw ,cfh) (corfu-popupinfo--frame-geometry corfu--frame))
               (cf-on-cursor-bottom
                (>= cfy
                    (+ (cadr (window-inside-pixel-edges))
                       (window-tab-line-height)
                       (or (cdr (posn-x-y (posn-at-point (point)))) 0)
                       lh)))
               ;; (y-on-top (max 0 (- cfy height border)))
               (y-on-bottom (+ cfy cfh (- border)))
               (h-remaining-top (- cfy border border))
               (h-remaining-bottom (- pfh y-on-bottom border border))
               (w-avail (min width (- pfw cfx border border))))
    ;; TODO cleanup
    (if cf-on-cursor-bottom
        (progn
          (setq height (min h-remaining-bottom height)
                height (min height (* (floor (/ height lh)) lh)))
          (list cfx y-on-bottom w-avail height 'bottom))
      (setq height (min h-remaining-top height)
            height (min height (* (floor (/ height lh)) lh)))
      (list cfx
            (max 0 (- cfy height border))
            w-avail height 'top))))

(defun corfu-popupinfo--display-area (dir width height)
  "Calculate the display area for the info popup.

If DIR is non-nil, the display area in the corresponding
direction is calculated first, its value can be 'bottom,
'top,'right or 'left.

The pixel size of the info popup can be specified by WIDTH and HEIGHT.

The calculated area is in the form (X Y WIDTH HEIGHT DIR).
DIR indicates the position direction of the info popup relative to
the candidate popup, its value is 'bottom, 'top, 'right or 'left."
  ;; TODO Direction handling is incomplete. Fix not only horizontal/vertical,
  ;; but left/right/bottom/top.
  (cond
   ((memq dir '(right left))
    (let ((size (corfu-popupinfo--size)))
      (corfu-popupinfo--display-area-horizontal (car size) (cdr size))))
   ((memq dir '(bottom top))
    (let ((size (corfu-popupinfo--size)))
      (corfu-popupinfo--display-area-vertical (car size) (cdr size))))
   (t
    (pcase-let* ((`(,width . ,height)
                  (if (and width height)
                      (cons width height)
                    (corfu-popupinfo--size)))
                 ((and h-a `(,_h-x ,_h-y ,h-w ,h-h ,_h-d))
                  (corfu-popupinfo--display-area-horizontal width height))
                 ((and v-a `(,_v-x ,_v-y ,v-w ,v-h ,_v-d))
                  (corfu-popupinfo--display-area-vertical width height)))
      (if (and (or (< h-h height) (< h-w width))
               (or (>= (* v-w v-h) (* h-w h-h))
                   (and (>= v-h height) (>= v-w width))))
          v-a h-a)))))

(defun corfu-popupinfo--show (candidate)
  "Show the info popup for CANDIDATE."
  (when corfu-popupinfo--auto-timer
    (cancel-timer corfu-popupinfo--auto-timer)
    (setq corfu-popupinfo--auto-timer nil))
  (when (and (corfu--popup-support-p) (corfu-popupinfo--visible-p corfu--frame))
    (let* ((doc-changed
            (not (and (corfu-popupinfo--visible-p)
                      (equal candidate corfu-popupinfo--candidate))))
           (new-coords (frame-edges corfu--frame 'inner-edges))
           (coords-changed (not (equal new-coords corfu-popupinfo--coordinates))))
      (when doc-changed
        (if-let (doc (funcall corfu-popupinfo--function candidate))
            (with-current-buffer (corfu--make-buffer " *corfu-popupinfo*" doc)
              (dolist (var corfu-popupinfo--buffer-parameters)
                (set (make-local-variable (car var)) (cdr var)))
              (setf face-remapping-alist (copy-tree face-remapping-alist)
                    (alist-get 'default face-remapping-alist) 'corfu-popupinfo))
          (corfu-popupinfo--hide)
          (setq doc-changed nil coords-changed nil)))
      (when (or doc-changed coords-changed)
        (pcase-let* ((border (alist-get 'child-frame-border-width corfu--frame-parameters))
                     (`(,area-x ,area-y ,area-w ,area-h ,area-d)
                      (corfu-popupinfo--display-area
                       corfu-popupinfo--direction
                       (and (not doc-changed)
                            (- (frame-pixel-width corfu-popupinfo--frame) border border))
                       (and (not doc-changed)
                            (- (frame-pixel-height corfu-popupinfo--frame) border border)))))
          (setq corfu-popupinfo--frame
                (corfu--make-frame corfu-popupinfo--frame
                                   area-x area-y area-w area-h
                                   " *corfu-popupinfo*")
                corfu-popupinfo--direction area-d
                corfu-popupinfo--candidate candidate
                corfu-popupinfo--coordinates new-coords))))))

(defun corfu-popupinfo--hide ()
  "Clear the info popup buffer content and hide it."
  (corfu--hide-frame corfu-popupinfo--frame))

(defun corfu-popupinfo-scroll-up (&optional n)
  "Scroll text of info popup window upward N lines.

If ARG is omitted or nil, scroll upward by a near full screen.
See `scroll-up' for details."
  (interactive "p")
  (if (corfu-popupinfo--visible-p)
      (with-selected-frame corfu-popupinfo--frame
        (with-current-buffer " *corfu-popupinfo*"
          (scroll-up n)))
    (scroll-other-window n)))

(defun corfu-popupinfo-scroll-down (&optional n)
  "Scroll text of info popup window down N lines.

If ARG is omitted or nil, scroll down by a near full screen."
  (interactive "p")
  (corfu-popupinfo-scroll-up (- (or n 1))))

(defun corfu-popupinfo--set-function (fun)
  "Set popup documentation getter FUN."
  (setq corfu-popupinfo--function fun
        corfu-popupinfo--candidate nil
        corfu-popupinfo--toggle nil)
  (when-let (candidate (and (>= corfu--index 0)
                            (nth corfu--index corfu--candidates)))
    (setq corfu-popupinfo--toggle t)
    (corfu-popupinfo--show candidate)))

(defun corfu-popupinfo-documentation ()
  "Show documentation in popup."
  (interactive)
  (corfu-popupinfo--set-function #'corfu-popupinfo--get-documentation))

(defun corfu-popupinfo-location ()
  "Show location in popup."
  (interactive)
  (corfu-popupinfo--set-function #'corfu-popupinfo--get-location))

(defun corfu-popupinfo-toggle ()
  "Toggle the info popup display or hide.

When using this command to manually hide the info popup, it will
not be displayed until this command is called again, even if
`corfu-popupinfo-delay' is non-nil."
  (interactive)
  (setq corfu-popupinfo--toggle nil)
  (if-let ((candidate (and (>= corfu--index 0)
                          (nth corfu--index corfu--candidates)))
           ((not (corfu-popupinfo--visible-p))))
      (setq corfu-popupinfo--toggle t)
      (corfu-popupinfo--show candidate)
    (corfu-popupinfo--hide)))

(defun corfu-popupinfo--exhibit (&rest _)
  "Update the info popup automatically."
  (add-to-list 'minor-mode-overriding-map-alist
               `(,#'corfu-popupinfo-mode . ,corfu-popupinfo-map))
  (if (and (>= corfu--index 0) (corfu-popupinfo--visible-p corfu--frame))
      (when (and corfu-popupinfo-delay corfu-popupinfo--toggle)
        (when corfu-popupinfo--auto-timer
          (cancel-timer corfu-popupinfo--auto-timer)
          (setq corfu-popupinfo--auto-timer nil))
        (let ((candidate (nth corfu--index corfu--candidates))
              (delay (if (consp corfu-popupinfo-delay)
                         (funcall (if (corfu-popupinfo--visible-p) #'cdr #'car)
                                  corfu-popupinfo-delay)
                       corfu-popupinfo-delay)))
          (if (or (eq delay t) (<= delay 0)
                  (equal candidate corfu-popupinfo--candidate))
              (corfu-popupinfo--show candidate)
            (cond
             (corfu-popupinfo-hide
              (corfu-popupinfo--hide))
             (corfu-popupinfo--candidate
              (corfu-popupinfo--show corfu-popupinfo--candidate)))
            (setq corfu-popupinfo--auto-timer
                  (run-at-time delay nil #'corfu-popupinfo--show candidate)))))
    (corfu-popupinfo--hide)))

(defun corfu-popupinfo--teardown ()
  "Teardown the info popup state."
  (corfu-popupinfo--hide)
  (mapc #'kill-local-variable corfu-popupinfo--state-vars)
  (setq minor-mode-overriding-map-alist
        (assq-delete-all #'corfu-popupinfo-mode
                         minor-mode-overriding-map-alist)))

;;;###autoload
(define-minor-mode corfu-popupinfo-mode
  "Corfu info popup minor mode."
  :global t :group 'corfu
  (cond
   (corfu-popupinfo-mode
    (advice-add #'corfu--exhibit :after #'corfu-popupinfo--exhibit)
    (advice-add #'corfu--teardown :before #'corfu-popupinfo--teardown))
   (t
    (advice-remove #'corfu--exhibit #'corfu-popupinfo--exhibit)
    (advice-remove #'corfu--teardown #'corfu-popupinfo--teardown))))

;; Emacs 28: Do not show Corfu commands with M-X
(dolist (sym '(corfu-popupinfo-scroll-down corfu-popupinfo-scroll-down
               corfu-popupinfo-documentation corfu-popupinfo-location
               corfu-popupinfo-toggle))
  (put sym 'completion-predicate #'ignore))

(provide 'corfu-popupinfo)
;;; corfu-popupinfo.el ends here
