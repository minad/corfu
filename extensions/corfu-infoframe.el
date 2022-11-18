;;; corfu-infoframe.el --- Candidate information popup frame for Corfu -*- lexical-binding: t -*-

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
;; Corfu. The `corfu-infoframe-mode' must be enabled globally. Set
;; `corfu-infoframe-auto' if you want the documentation popup frame to
;; be displayed automatically.

;; For manual toggling the commands `corfu-infoframe-toggle',
;; `corfu-infoframe-location' and `corfu-infoframe-documentation' are
;; bound in the `corfu-infoframe-map'.

;;; Code:

(require 'corfu)
(eval-when-compile
  (require 'subr-x))

(defface corfu-infoframe
  '((t :inherit corfu-default :height 0.8))
  "Face used for the info frame."
  :group 'corfu-faces)

(defcustom corfu-infoframe-auto t
  "Display documentation popup automatically."
  :group 'corfu
  :type 'boolean)

(defcustom corfu-infoframe-delay 1.0
  "The number of seconds to wait before displaying the documentation popup."
  :group 'corfu
  :type '(choice (const :tag "immediate" 0)
                 (number :tag "seconds")))

(defcustom corfu-infoframe-hide t
  "Hide the popup during the transition between candidates."
  :group 'corfu
  :type 'boolean)

(defcustom corfu-infoframe-max-width 50
  "The max width of the info frame in characters."
  :group 'corfu
  :type 'integer)

(defcustom corfu-infoframe-max-height 10
  "The max height of the info frame in characters."
  :group 'corfu
  :type 'integer)

(defcustom corfu-infoframe-resize t
  "Resize the info frame automatically if non-nil."
  :group 'corfu
  :type 'boolean)

(defvar corfu-infoframe-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-d" #'corfu-infoframe-documentation)
    (define-key map "\M-l" #'corfu-infoframe-location)
    (define-key map "\M-t" #'corfu-infoframe-toggle)
    (define-key map [remap scroll-other-window] #'corfu-infoframe-scroll-up)
    (define-key map [remap scroll-other-window-down] #'corfu-infoframe-scroll-down)
    map)
  "Additional keymap activated in infoframe mode.")

(defvar-local corfu-infoframe--toggle t
  "Local infoframe toggle state.")

(defvar-local corfu-infoframe--function
  #'corfu-infoframe--get-documentation
  "Documentation function.")

(defvar corfu-infoframe--frame nil
  "Info frame.")

(defvar corfu-infoframe--auto-timer nil
  "Corfu info frame auto display timer.")

(defvar-local corfu-infoframe--candidate nil
  "Completion candidate for the info frame.")

(defvar-local corfu-infoframe--edges nil
  "Coordinates of the corfu popup's edges.

The coordinates list has the form (LEFT TOP RIGHT BOTTOM) where all
values are in pixels relative to the origin - the position (0, 0)
- of FRAME's display.  For terminal frames all values are
relative to LEFT and TOP which are both zero.
See `frame-edges' for details.")

(defvar-local corfu-infoframe--direction nil
  "Position direction of the info frame relative to the corfu popup.")

(defconst corfu-infoframe--state-vars
  '(corfu-infoframe--candidate
    corfu-infoframe--edges
    corfu-infoframe--direction
    corfu-infoframe--toggle
    corfu-infoframe--function)
  "Buffer-local state variables used by corfu-infoframe.")

(defun corfu-infoframe--visible-p ()
  "Determine whether the info frame is visible."
  (and (frame-live-p corfu-infoframe--frame)
       (frame-visible-p corfu-infoframe--frame)))

(defun corfu-infoframe--get-location (candidate)
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
              (forward-line (* 2 corfu-infoframe-max-height))
              (when jit-lock-mode
                (jit-lock-fontify-now beg (point)))
              (setq res (buffer-substring beg (point)))
              (and (not (string-blank-p res)) res))))))))

(defun corfu-infoframe--get-documentation (candidate)
  "Get the documentation for CANDIDATE."
  (when-let* ((fun (plist-get corfu--extra :company-doc-buffer))
              (res (save-excursion
                     (let ((inhibit-message t)
                           (message-log-max nil)
                           ;; Reduce print length for elisp backend (#249)
                           (print-level 3)
                           (print-length (* corfu-infoframe-max-width
                                            corfu-infoframe-max-height)))
                       (funcall fun candidate)))))
    (with-current-buffer (or (car-safe res) res)
      (setq res (replace-regexp-in-string
                 "[\\s-\n]*\\[back\\][\\s-\n]*" ""
                 (buffer-string)))
      (and (not (string-blank-p res)) res))))

;; TODO get rid of optional arguments?
(defun corfu-infoframe--size (&optional width height)
  "Calculate popup size in the form of (width height).

If WIDTH and HEIGHT is speicified, just return (WIDTH HEIGHT)."
  (let ((max-width (* (frame-char-width) corfu-infoframe-max-width))
        (max-height (* (default-line-height) corfu-infoframe-max-height)))
    (if (and width height)
        (list (min width max-width) (min height max-height))
      (pcase-let* ((`(,popup-width ,popup-height)
                    (if (not corfu-infoframe-resize)
                        (list (or width max-width) (or height max-height))
                      (pcase-let ((`(,win-width . ,win-height)
                                   (save-window-excursion
                                     (with-current-buffer " *corfu-infoframe*"
                                       (set-window-dedicated-p nil nil)
                                       (set-window-buffer nil (current-buffer))
                                       (window-text-pixel-size nil (point-min) (point-max)
                                                               (* 2 max-width) (* 2 max-height))))))
                        (list (or width win-width) (or height win-height))))))
        (list (min popup-width max-width) (min popup-height max-height))))))

(defun corfu-infoframe--frame-geometry (frame)
  "Return position and size geometric attributes of FRAME.

The geometry represents the position and size in pixels
in the form of (X Y WIDTH HEIGHT)."
  (pcase-let ((`(,x . ,y) (frame-position frame)))
    (list x y (frame-pixel-width frame) (frame-pixel-height frame))))

(defun corfu-infoframe--display-area-horizontal (width height)
  "Calculate the horizontal display area for the info frame.

The WIDTH and HEIGHT of the info frame are in pixels.

The calculated area is in the form (X Y WIDTH HEIGHT DIRECTION).
DIRECTION indicates the horizontal position direction of the info frame
relative to the corfu popup, its value can be 'right or 'left."
  (pcase-let* ((border (alist-get 'child-frame-border-width corfu--frame-parameters))
               ;; space between candidates popup and info frame
               (space (- border))  ;; share the border
               (`(,_pfx ,_pfy ,pfw ,_pfh)
                (corfu-infoframe--frame-geometry (frame-parent corfu--frame)))
               (`(,cfx ,cfy ,cfw ,_cfh) (corfu-infoframe--frame-geometry corfu--frame))
               (x-on-right (+ cfx cfw space))
               (w-remaining-right (- pfw 1 x-on-right border border))
               (x-on-left (- cfx space pfw))
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

(defun corfu-infoframe--display-area-vertical (width height)
  "Calculate the vertical display area for the info frame.

The WIDTH and HEIGHT of the info frame are in pixels.

The calculated area is in the form (X Y WIDTH HEIGHT DIRECTION).
DIRECTION indicates the vertical position direction of the info frame
relative to the corfu popup, its value can be 'bottom or 'top."
  (pcase-let* ((a-y 0) (a-height height) (a-direction 'bottom)
               (border (alist-get 'child-frame-border-width corfu--frame-parameters))
               (space (- border))
               (lh (default-line-height))
               (`(,_pfx ,_pfy ,pfw ,pfh)
                (corfu-infoframe--frame-geometry (frame-parent corfu--frame)))
               (`(,cfx ,cfy ,_cfw ,cfh) (corfu-infoframe--frame-geometry corfu--frame))
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

(defun corfu-infoframe--display-area (direction width height)
  "Calculate the display area for the info frame.

If DIRECTION is non-nil, the display area in the corresponding
direction is calculated first, its value can be 'bottom,
'top,'right or 'left.

The pixel size of the info frame can be specified by WIDTH and HEIGHT.

The calculated area is in the form (X Y WIDTH HEIGHT DIRECTION).
DIRECTION indicates the position direction of the info frame relative to
the corfu popup, its value is 'bottom, 'top, 'right or 'left."
  ;; TODO wrong
  (cond
   ((member direction '(right left))
    (apply #'corfu-infoframe--display-area-horizontal
           (corfu-infoframe--size)))
   ((member direction '(bottom top))
    (apply #'corfu-infoframe--display-area-vertical
           (corfu-infoframe--size)))
   (t
    (pcase-let* ((`(,width ,height)  ;; popup inner width and height
                  (corfu-infoframe--size width height))
                 ((and h-a `(,_h-x ,_h-y ,h-w ,h-h ,_h-d))
                  (corfu-infoframe--display-area-horizontal width height))
                 ((and v-a `(,_v-x ,_v-y ,v-w ,v-h ,_v-d))
                  (corfu-infoframe--display-area-vertical width height)))
      (if (and (or (< h-h height) (< h-w width))
               (or (>= (* v-w v-h) (* h-w h-h))
                   (and (>= v-h height) (>= v-w width))))
          v-a h-a)))))

(defun corfu-infoframe--show (candidate)
  "Show the info frame for CANDIDATE."
  (when corfu-infoframe--auto-timer
    (cancel-timer corfu-infoframe--auto-timer)
    (setq corfu-infoframe--auto-timer nil))
  (when (and (corfu--popup-support-p)
             (frame-live-p corfu--frame)
             (frame-visible-p corfu--frame))
    (let* ((doc-changed
            (not (and (corfu-infoframe--visible-p)
                      (equal candidate corfu-infoframe--candidate))))
           (new-edges (frame-edges corfu--frame 'inner-edges))
           (edges-changed (not (equal new-edges corfu-infoframe--edges))))
      (when doc-changed
        (if-let (doc (funcall corfu-infoframe--function candidate))
            (with-current-buffer (corfu--make-buffer " *corfu-infoframe*" doc)
              ;; TODO extract settings
              (setq-local line-move-visual t
                          truncate-partial-width-windows nil
                          left-margin-width 1
                          right-margin-width 1
                          truncate-lines nil
                          word-wrap t
                          fringe-indicator-alist '((continuation))
                          face-remapping-alist (copy-tree face-remapping-alist))
              (setf (alist-get 'default face-remapping-alist) 'corfu-infoframe))
          (corfu-infoframe--hide)
          (setq doc-changed nil edges-changed nil)))
      (when (or doc-changed edges-changed)
        (pcase-let* ((border (alist-get 'child-frame-border-width corfu--frame-parameters))
                     (`(,area-x ,area-y ,area-w ,area-h ,area-d)
                      (corfu-infoframe--display-area
                       corfu-infoframe--direction
                       (and (not doc-changed)
                            (- (frame-pixel-width corfu-infoframe--frame) border border))
                       (and (not doc-changed)
                            (- (frame-pixel-height corfu-infoframe--frame) border border)))))
          (setq corfu-infoframe--frame
                (corfu--make-frame corfu-infoframe--frame
                                   area-x area-y area-w area-h
                                   (get-buffer " *corfu-infoframe*"))
                corfu-infoframe--direction area-d)))
      (setq corfu-infoframe--candidate candidate
            corfu-infoframe--edges new-edges))))

(defun corfu-infoframe--hide ()
  "Clear the info frame buffer content and hide it."
  (corfu--hide-frame corfu-infoframe--frame))

(defun corfu-infoframe-scroll-up (&optional n)
  "Scroll text of info frame window upward N lines.

If ARG is omitted or nil, scroll upward by a near full screen.
See `scroll-up' for details."
  (interactive "p")
  (when (corfu-infoframe--visible-p)
    (with-selected-frame corfu-infoframe--frame
      (with-current-buffer (get-buffer " *corfu-infoframe*")
        (scroll-up n)))))

(defun corfu-infoframe-scroll-down (&optional n)
  "Scroll text of info frame window down N lines.

If ARG is omitted or nil, scroll down by a near full screen."
  (interactive "p")
  (corfu-infoframe-scroll-up (- (or n 1))))

(defun corfu-infoframe--set-function (fun)
  "Set popup documentation getter FUN."
  (setq corfu-infoframe--function fun
        corfu-infoframe--candidate nil
        corfu-infoframe--toggle t)
  (when-let (candidate (and (>= corfu--index 0)
                            (nth corfu--index corfu--candidates)))
    (corfu-infoframe--show candidate)))

(defun corfu-infoframe-documentation ()
  "Show documentation in popup."
  (interactive)
  (corfu-infoframe--set-function #'corfu-infoframe--get-documentation))

(defun corfu-infoframe-location ()
  "Show location in popup."
  (interactive)
  (corfu-infoframe--set-function #'corfu-infoframe--get-location))

(defun corfu-infoframe-toggle ()
  "Toggle the info frame display or hide.

When using this command to manually hide the info frame, it will
not be displayed until this command is called again, even if
`corfu-infoframe-auto' is non-nil."
  (interactive)
  (if-let ((candidate (and (>= corfu--index 0)
                          (nth corfu--index corfu--candidates)))
           ((setq corfu-infoframe--toggle (not (corfu-infoframe--visible-p)))))
      (corfu-infoframe--show candidate)
    (corfu-infoframe--hide)))

(defun corfu-infoframe--exhibit (&rest _)
  "Update the info frame automatically."
  (add-to-list 'minor-mode-overriding-map-alist
               `(,#'corfu-infoframe-mode . ,corfu-infoframe-map))
  (if (and (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame)
           (>= corfu--index 0))
      (when (and corfu-infoframe-auto corfu-infoframe--toggle)
        (when corfu-infoframe--auto-timer
          (cancel-timer corfu-infoframe--auto-timer)
          (setq corfu-infoframe--auto-timer nil))
        (let ((candidate (nth corfu--index corfu--candidates)))
          (if (or (= corfu-infoframe-delay 0)
                  (equal candidate corfu-infoframe--candidate))
              (corfu-infoframe--show candidate)
            (if corfu-infoframe-hide
                (corfu-infoframe--hide)
              (corfu-infoframe--show corfu-infoframe--candidate))
            (setq corfu-infoframe--auto-timer
                  (run-at-time corfu-infoframe-delay nil
                               #'corfu-infoframe--show candidate)))))
    (corfu-infoframe--hide)))

(defun corfu-infoframe--teardown ()
  "Teardown the info frame state."
  (corfu-infoframe--hide)
  (mapc #'kill-local-variable corfu-infoframe--state-vars)
  (setq minor-mode-overriding-map-alist
        (assq-delete-all #'corfu-infoframe-mode
                         minor-mode-overriding-map-alist)))

;;;###autoload
(define-minor-mode corfu-infoframe-mode
  "Corfu info frame minor mode."
  :global t :group 'corfu
  (cond
   (corfu-infoframe-mode
    (advice-add #'corfu--exhibit :after #'corfu-infoframe--exhibit)
    (advice-add #'corfu--teardown :before #'corfu-infoframe--teardown))
   (t
    (advice-remove #'corfu--exhibit #'corfu-infoframe--exhibit)
    (advice-remove #'corfu--teardown #'corfu-infoframe--teardown))))

(provide 'corfu-infoframe)
;;; corfu-infoframe.el ends here
