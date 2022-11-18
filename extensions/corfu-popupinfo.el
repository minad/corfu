;;; corfu-popupinfo.el --- Candidate information popup for Corfu -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

;; Author: Yuwei Tian <fishtai0@gmail.com>, Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (corfu "0.28"))
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
;; experimental. The public interface may change any time.
;;
;; Display a documentation popup for completion candidate when using
;; Corfu. The `corfu-popupinfo-mode' must be enabled globally. Set
;; `corfu-popupinfo-auto' if you want the documentation popup to be
;; displayed automatically.

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

(defcustom corfu-popupinfo-auto t
  "Display info popup automatically."
  :group 'corfu
  :type 'boolean)

(defcustom corfu-popupinfo-delay 1.0
  "The number of seconds to wait before displaying the documentation popup."
  :group 'corfu
  :type '(choice (const :tag "immediate" 0)
                 (number :tag "seconds")))

(defcustom corfu-popupinfo-hide t
  "Hide the popup during the transition between candidates."
  :group 'corfu
  :type 'boolean)

(defcustom corfu-popupinfo-max-width 50
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

(defvar-local corfu-popupinfo--edges nil
  "Coordinates of the candidate popup edges.

The coordinates list has the form (LEFT TOP RIGHT BOTTOM) where all
values are in pixels relative to the origin - the position (0, 0)
- of FRAME's display.  For terminal frames all values are
relative to LEFT and TOP which are both zero.
See `frame-edges' for details.")

(defvar-local corfu-popupinfo--direction nil
  "Position direction of the info popup relative to the candidate popup.")

(defconst corfu-popupinfo--state-vars
  '(corfu-popupinfo--candidate
    corfu-popupinfo--edges
    corfu-popupinfo--direction
    corfu-popupinfo--toggle
    corfu-popupinfo--function)
  "Buffer-local state variables used by corfu-popupinfo.")

(defun corfu-popupinfo--visible-p ()
  "Determine whether the info popup is visible."
  (and (frame-live-p corfu-popupinfo--frame)
       (frame-visible-p corfu-popupinfo--frame)))

(defun corfu-popupinfo--get-location (candidate)
  "Get source at location of CANDIDATE."
  (save-excursion
    (when-let* ((fun (plist-get corfu--extra :company-location))
                (loc (funcall fun candidate))
                (res (or (and (bufferp (car loc)) (car loc)) (find-file-noselect (car loc) t))))
      (with-current-buffer res
        (save-excursion
          (save-restriction
            (widen)
            (if (bufferp (car loc))
                (goto-char (cdr loc))
              (goto-char (point-min))
              (forward-line (1- (cdr loc))))
            (let ((beg (point)))
              (forward-line (* 2 corfu-popupinfo-max-height))
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

;; TODO get rid of optional arguments?
(defun corfu-popupinfo--size (&optional width height)
  "Calculate popup size in the form of (width height).

If WIDTH and HEIGHT is speicified, just return (WIDTH HEIGHT)."
  (let ((max-width (* (frame-char-width) corfu-popupinfo-max-width))
        (max-height (* (default-line-height) corfu-popupinfo-max-height)))
    (if (and width height)
        (list (min width max-width) (min height max-height))
      (pcase-let* ((`(,popup-width ,popup-height)
                    (if (not corfu-popupinfo-resize)
                        (list (or width max-width) (or height max-height))
                      (pcase-let ((`(,win-width . ,win-height)
                                   (save-window-excursion
                                     (with-current-buffer " *corfu-popupinfo*"
                                       (set-window-dedicated-p nil nil)
                                       (set-window-buffer nil (current-buffer))
                                       (window-text-pixel-size nil (point-min) (point-max)
                                                               (* 2 max-width) (* 2 max-height))))))
                        (list (or width win-width) (or height win-height))))))
        (list (min popup-width max-width) (min popup-height max-height))))))

(defun corfu-popupinfo--frame-geometry (frame)
  "Return position and size geometric attributes of FRAME.

The geometry represents the position and size in pixels
in the form of (X Y WIDTH HEIGHT)."
  (pcase-let ((`(,x . ,y) (frame-position frame)))
    (list x y (frame-pixel-width frame) (frame-pixel-height frame))))

(defun corfu-popupinfo--display-area-horizontal (width height)
  "Calculate the horizontal display area for the info popup.

The WIDTH and HEIGHT of the info popup are in pixels.
The calculated area is in the form (X Y WIDTH HEIGHT DIRECTION).
DIRECTION indicates the horizontal position direction of the info popup
relative to the candidate popup, its value can be 'right or 'left."
  (pcase-let* ((border (alist-get 'child-frame-border-width corfu--frame-parameters))
               ;; space between candidates popup and info popup
               (space (- border))  ;; share the border
               (`(,_pfx ,_pfy ,pfw ,_pfh)
                (corfu-popupinfo--frame-geometry (frame-parent corfu--frame)))
               (`(,cfx ,cfy ,cfw ,_cfh) (corfu-popupinfo--frame-geometry corfu--frame))
               (x-on-right (+ cfx cfw space))
               (w-remaining-right (- pfw 1 x-on-right border border))
               (x-on-left (- cfx space border width border))
               (w-remaining-left (- cfx space 1 border border)))
    (cond
     ((> w-remaining-right width)
      (list x-on-right cfy width height 'right))
     ((and (< w-remaining-right width)
           (> w-remaining-left width))
      (list x-on-left cfy width height 'left))
     ((>= w-remaining-right w-remaining-left)
      (list x-on-right cfy w-remaining-right height 'right))
     (t
      (list x-on-left cfy w-remaining-left height 'left)))))

(defun corfu-popupinfo--display-area-vertical (width height)
  "Calculate the vertical display area for the info popup.

The WIDTH and HEIGHT of the info popup are in pixels.

The calculated area is in the form (X Y WIDTH HEIGHT DIRECTION).
DIRECTION indicates the vertical position direction of the info popup
relative to the candidate popup, its value can be 'bottom or 'top."
  (pcase-let* ((a-y 0) (a-height height) (a-direction 'bottom)
               (border (alist-get 'child-frame-border-width corfu--frame-parameters))
               (space (- border))
               (lh (default-line-height))
               (`(,_pfx ,_pfy ,pfw ,pfh)
                (corfu-popupinfo--frame-geometry (frame-parent corfu--frame)))
               (`(,cfx ,cfy ,_cfw ,cfh) (corfu-popupinfo--frame-geometry corfu--frame))
               (cf-on-cursor-bottom-p
                (>= cfy
                    (+ (cadr (window-inside-pixel-edges))
                       (window-tab-line-height)
                       (or (cdr (posn-x-y (posn-at-point (point)))) 0)
                       lh)))
               (y-on-top (max 0 (- cfy space border height border)))
               (h-remaining-top (- cfy border border))
               (y-on-bottom (+ cfy cfh space))
               (h-remaining-bottom (- pfh y-on-bottom border border))
               (a-width (min width (- pfw cfx border border))))
    ;; TODO cleanup, get rid of a-* variables
    (if cf-on-cursor-bottom-p
        (setq a-y y-on-bottom
              a-height (min h-remaining-bottom height))
      (setq a-y y-on-top
            a-height (min h-remaining-top height)
            a-direction 'top))
    (setq a-height (min a-height (* (floor (/ a-height lh)) lh)))
    (unless cf-on-cursor-bottom-p
      (setq a-y (max 0 (- cfy space border height border))))
    (list cfx a-y a-width a-height a-direction)))

(defun corfu-popupinfo--display-area (direction width height)
  "Calculate the display area for the info popup.

If DIRECTION is non-nil, the display area in the corresponding
direction is calculated first, its value can be 'bottom,
'top,'right or 'left.

The pixel size of the info popup can be specified by WIDTH and HEIGHT.

The calculated area is in the form (X Y WIDTH HEIGHT DIRECTION).
DIRECTION indicates the position direction of the info popup relative to
the candidate popup, its value is 'bottom, 'top, 'right or 'left."
  ;; TODO wrong
  (cond
   ((member direction '(right left))
    (apply #'corfu-popupinfo--display-area-horizontal
           (corfu-popupinfo--size)))
   ((member direction '(bottom top))
    (apply #'corfu-popupinfo--display-area-vertical
           (corfu-popupinfo--size)))
   (t
    (pcase-let* ((`(,width ,height)  ;; popup inner width and height
                  (corfu-popupinfo--size width height))
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
  (when (and (corfu--popup-support-p)
             (frame-live-p corfu--frame)
             (frame-visible-p corfu--frame))
    (let* ((doc-changed
            (not (and (corfu-popupinfo--visible-p)
                      (equal candidate corfu-popupinfo--candidate))))
           (new-edges (frame-edges corfu--frame 'inner-edges))
           (edges-changed (not (equal new-edges corfu-popupinfo--edges))))
      (when doc-changed
        (if-let (doc (funcall corfu-popupinfo--function candidate))
            (with-current-buffer (corfu--make-buffer " *corfu-popupinfo*" doc)
              ;; TODO extract settings
              (setq-local line-move-visual t
                          truncate-partial-width-windows nil
                          left-margin-width 1
                          right-margin-width 1
                          truncate-lines nil
                          word-wrap t
                          fringe-indicator-alist '((continuation))
                          face-remapping-alist (copy-tree face-remapping-alist))
              (setf (alist-get 'default face-remapping-alist) 'corfu-popupinfo))
          (corfu-popupinfo--hide)
          (setq doc-changed nil edges-changed nil)))
      (when (or doc-changed edges-changed)
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
                                   (get-buffer " *corfu-popupinfo*"))
                corfu-popupinfo--direction area-d)))
      (setq corfu-popupinfo--candidate candidate
            corfu-popupinfo--edges new-edges))))

(defun corfu-popupinfo--hide ()
  "Clear the info popup buffer content and hide it."
  (corfu--hide-frame corfu-popupinfo--frame))

(defun corfu-popupinfo-scroll-up (&optional n)
  "Scroll text of info popup window upward N lines.

If ARG is omitted or nil, scroll upward by a near full screen.
See `scroll-up' for details."
  (interactive "p")
  (when (corfu-popupinfo--visible-p)
    (with-selected-frame corfu-popupinfo--frame
      (with-current-buffer (get-buffer " *corfu-popupinfo*")
        (scroll-up n)))))

(defun corfu-popupinfo-scroll-down (&optional n)
  "Scroll text of info popup window down N lines.

If ARG is omitted or nil, scroll down by a near full screen."
  (interactive "p")
  (corfu-popupinfo-scroll-up (- (or n 1))))

(defun corfu-popupinfo--set-function (fun)
  "Set popup documentation getter FUN."
  (setq corfu-popupinfo--function fun
        corfu-popupinfo--candidate nil
        corfu-popupinfo--toggle t)
  (when-let (candidate (and (>= corfu--index 0)
                            (nth corfu--index corfu--candidates)))
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
`corfu-popupinfo-auto' is non-nil."
  (interactive)
  (if-let ((candidate (and (>= corfu--index 0)
                          (nth corfu--index corfu--candidates)))
           ((setq corfu-popupinfo--toggle (not (corfu-popupinfo--visible-p)))))
      (corfu-popupinfo--show candidate)
    (corfu-popupinfo--hide)))

(defun corfu-popupinfo--exhibit (&rest _)
  "Update the info popup automatically."
  (add-to-list 'minor-mode-overriding-map-alist
               `(,#'corfu-popupinfo-mode . ,corfu-popupinfo-map))
  (if (and (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame)
           (>= corfu--index 0))
      (when (and corfu-popupinfo-auto corfu-popupinfo--toggle)
        (when corfu-popupinfo--auto-timer
          (cancel-timer corfu-popupinfo--auto-timer)
          (setq corfu-popupinfo--auto-timer nil))
        (let ((candidate (nth corfu--index corfu--candidates)))
          (if (or (= corfu-popupinfo-delay 0)
                  (equal candidate corfu-popupinfo--candidate))
              (corfu-popupinfo--show candidate)
            (if corfu-popupinfo-hide
                (corfu-popupinfo--hide)
              (corfu-popupinfo--show corfu-popupinfo--candidate))
            (setq corfu-popupinfo--auto-timer
                  (run-at-time corfu-popupinfo-delay nil
                               #'corfu-popupinfo--show candidate)))))
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
