;;; corfu-docframe.el --- Documentation popup for Corfu -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

;; Author: Yuwei Tian <fishtai0@gmail.com>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Keywords: corfu popup documentation convenience
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

;; Display a documentation popup for completion candidate when using Corfu.
;;
;; Enable `corfu-docframe-mode':
;;   (add-hook 'corfu-mode-hook #'corfu-docframe-mode).

;;; Code:

(require 'corfu)
(eval-when-compile
  (require 'subr-x))

(defcustom corfu-docframe-auto t
  "Display documentation popup automatically."
  :group 'corfu
  :type 'boolean)

(defcustom corfu-docframe-delay 1.0
  "The number of seconds to wait before displaying the documentation popup.

The value of nil means no delay."
  :group 'corfu
  :type '(choice (const :tag "never (nil)" nil)
          (const :tag "immediate (0)" 0)
          (number :tag "seconds")))

(defcustom corfu-docframe-hide t
  "Hide the popup during the transition between candidates."
  :group 'corfu
  :type 'boolean)

(defcustom corfu-docframe-max-width 80
  "The max width of the corfu doc popup in characters."
  :group 'corfu
  :type 'integer)

(defcustom corfu-docframe-max-height 10
  "The max height of the corfu doc popup in characters."
  :group 'corfu
  :type 'integer)

(defcustom corfu-docframe-resize-frame t
  "Non-nil means resize the corfu doc popup automatically.

If this is nil, do not resize corfu doc popup automatically."
  :group 'corfu
  :type 'boolean)

(defvar corfu-docframe--frame nil
  "Doc frame.")

(defvar corfu-docframe--frame-parameters
  (let* ((cw (default-font-width))
         (lmw (* cw corfu-left-margin-width))
         (rmw (* cw corfu-right-margin-width))
         (fp (copy-alist corfu--frame-parameters)))
    (setf (alist-get 'left-fringe fp) (ceiling lmw)
          (alist-get 'right-fringe fp) (ceiling rmw))
    fp)
  "Default doc child frame parameters.")

(defvar corfu-docframe--auto-timer nil
  "Corfu doc popup auto display timer.")

(defvar-local corfu-docframe--candidate nil
  "Completion candidate for the doc popup.")

(defvar-local corfu-docframe--cf-popup-edges nil
  "Coordinates of the corfu popup's edges.

The coordinates list has the form (LEFT TOP RIGHT BOTTOM) where all
values are in pixels relative to the origin - the position (0, 0)
- of FRAME's display.  For terminal frames all values are
relative to LEFT and TOP which are both zero.

See `frame-edges' for details.")

(defvar corfu-docframe--cf-window nil
  "Window where the corfu popup is located.")

(defvar-local corfu-docframe--direction nil
  "Position direction of the doc popup relative to the corfu popup.")

(defconst corfu-docframe--state-vars
  '(corfu-docframe--candidate
    corfu-docframe--cf-popup-edges
    corfu-docframe--cf-window
    corfu-docframe--direction)
  "Buffer-local state variables used by corfu-docframe.")

(defun corfu-docframe--visible-p ()
  "Determine whether the doc popup is visible."
  (and (frame-live-p corfu-docframe--frame)
       (frame-visible-p corfu-docframe--frame)))

(defun corfu-docframe--get-doc ()
  "Get the documentation for the current completion candidate.

The documentation is trimmed.
Returns nil if an error occurs or the documentation content is empty."
  (when-let
      ((doc
        (cond
          ((= corfu--total 0) nil)  ;; No candidates
          ((< corfu--index 0) nil)  ;; No candidate selected
          (t
           (if-let*
               ((fun (plist-get corfu--extra :company-doc-buffer))
                (res
                 ;; fix showing candidate location
                 ;; when fetch helpful documentation
                 (save-excursion
                   (let ((inhibit-message t)
                         (message-log-max nil))
                     (funcall fun (nth corfu--index corfu--candidates))))))
               (let ((buf (or (car-safe res) res)))
                 (with-current-buffer buf
                   (buffer-string)))
             nil)))))  ;; No documentation available
    (unless (string-empty-p (string-trim doc))
      doc)))

(defun corfu-docframe--size (&optional width height)
  "Calculate popup size in the form of (width height).

If WIDTH and HEIGHT is speicified, just return (WIDTH HEIGHT)."
  (let ((max-width (* (frame-char-width) corfu-docframe-max-width))
        (max-height (* (frame-char-height) corfu-docframe-max-height)))
    (if (and width height)
        (list (min width max-width) (min height max-height))
      (pcase-let*
          ((lfw (alist-get 'left-fringe corfu-docframe--frame-parameters))
           (rfw (alist-get 'right-fringe corfu-docframe--frame-parameters))
           (`(,popup-width ,popup-height)
             (if (not corfu-docframe-resize-frame)
                 (list (or width
                           ;; left margin + inner width + right margin
                           (+ lfw max-width rfw))
                       (or height max-height))
               (pcase-let
                   ((`(,win-width . ,win-height)
                      (save-window-excursion
                        (with-current-buffer " *corfu-docframe*"
                          (set-window-dedicated-p nil nil)
                          (set-window-buffer nil (current-buffer))
                          (window-text-pixel-size
                           nil (point-min) (point-max)
                           (* (default-font-width) corfu-docframe-max-width)
                           (* (default-line-height) corfu-docframe-max-height))))))
                 (list (or width win-width) (or height win-height))))))
        (list (min popup-width max-width) (min popup-height max-height))))))

(defun corfu-docframe--frame-geometry (&optional frame)
  "Return position and size geometric attributes of FRAME.

The geometry represents the position and size in pixels
in the form of (X Y WIDTH HEIGHT).

FRAME must be a live frame and defaults to the selected one."
  (pcase-let
      ((`(,x . ,y) (frame-position frame)))
    (list x y (frame-pixel-width frame) (frame-pixel-height frame))))

(defun corfu-docframe--display-area-horizontal (width height)
  "Calculate the horizontal display area for the doc popup.

The WIDTH and HEIGHT of the doc popup are in pixels.

The calculated area is in the form (X Y WIDTH HEIGHT DIRECTION).
DIRECTION indicates the horizontal position direction of the doc popup
relative to the corfu popup, its value can be 'right or 'left."
  (pcase-let*
      ((a-x 0) (a-y 0) (a-width width) (a-height height) (a-direction 'right)
       (border
        (alist-get 'child-frame-border-width corfu-docframe--frame-parameters))
       ;; space between candidates popup and doc popup
       (space (- border))  ;; share the border
       (lfw (alist-get 'left-fringe corfu-docframe--frame-parameters))
       (rfw (alist-get 'right-fringe corfu-docframe--frame-parameters))
       (`(,_pfx ,_pfy ,pfw ,_pfh)
         (corfu-docframe--frame-geometry (frame-parent corfu--frame)))
       (`(,cfx ,cfy ,cfw ,_cfh) (corfu-docframe--frame-geometry corfu--frame))
       (x-on-right (+ cfx cfw space))
       ;; width remaining right
       (w-remaining-right (- pfw 1 x-on-right border lfw rfw border))
       (x-on-left (- cfx space pfw))
       ;; width remaining left
       (w-remaining-left (- cfx space 1 border lfw rfw border)))
    (cond
      ((> w-remaining-right width)
       (setq a-x x-on-right))
      ((and (< w-remaining-right width)
            (> w-remaining-left width))
       (setq a-x x-on-left
             a-direction 'left)
       ;; workaround for emacs bug#58627
       (when (eq window-system 'ns)
         (setq a-x (- cfx space 1 border lfw width rfw border))))
      ((>= w-remaining-right w-remaining-left)
       (setq a-x x-on-right
             a-width w-remaining-right))
      (t
       (setq a-x x-on-left
             a-direction 'left
             a-width w-remaining-left)
       ;; workaround for emacs bug#58627
       (when (eq window-system 'ns)
         (setq a-x 1))))
    (setq a-y cfy)
    (list a-x a-y a-width a-height a-direction)))

(defun corfu-docframe--display-area-vertical (width height)
  "Calculate the vertical display area for the doc popup.

The WIDTH and HEIGHT of the doc popup are in pixels.

The calculated area is in the form (X Y WIDTH HEIGHT DIRECTION).
DIRECTION indicates the vertical position direction of the doc popup
relative to the corfu popup, its value can be 'bottom or 'top."
  (pcase-let*
      ((a-x 0) (a-y 0) (a-height height) (a-direction 'bottom)
       (border
        (alist-get 'child-frame-border-width
                   corfu-docframe--frame-parameters))
       (space (- border))
       (lfw (alist-get 'left-fringe corfu-docframe--frame-parameters))
       (rfw (alist-get 'right-fringe corfu-docframe--frame-parameters))
       (lh (default-line-height))
       (`(,_pfx ,_pfy ,pfw ,pfh)
         (corfu-docframe--frame-geometry (frame-parent corfu--frame)))
       (`(,cfx ,cfy ,_cfw ,cfh) (corfu-docframe--frame-geometry corfu--frame))
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
       (a-width (min width (- pfw cfx border lfw rfw border))))
    (if cf-on-cursor-bottom-p
        (setq a-y y-on-bottom
              a-height (min h-remaining-bottom height))
      (setq a-y y-on-top
            a-height (min h-remaining-top height)
            a-direction 'top))
    (setq a-height (min a-height (* (floor (/ a-height lh)) lh)))
    (unless cf-on-cursor-bottom-p
      (setq a-y (max 0 (- cfy space border height border))))
    (setq a-x cfx)
    (list a-x a-y a-width a-height a-direction)))

(defun corfu-docframe--display-area (&optional direction width height)
  "Calculate the display area for the doc popup.

If DIRECTION is specified, the display area in the corresponding direction
is calculated first, its value can be 'bottom, 'top,'right or 'left.

The pixel size of the doc popup can be specified with the optional
arguments WIDTH and HEIGHT.

The calculated area is in the form (X Y WIDTH HEIGHT DIRECTION).
DIRECTION indicates the position direction of the doc popup relative to
the corfu popup, its value is 'bottom, 'top, 'right or 'left."
  (cond
    ((member direction '(right left))
     (apply #'corfu-docframe--display-area-horizontal
            (corfu-docframe--size)))
    ((member direction '(bottom top))
     (apply #'corfu-docframe--display-area-vertical
            (corfu-docframe--size)))
    (t
     (pcase-let*
         ((`(,width ,height)  ;; popup inner width and height
            (corfu-docframe--size width height))
          (`(,v-x ,v-y ,v-w ,v-h ,v-d)
            (corfu-docframe--display-area-vertical width height)))
       (if (and (>= v-h height) (>= v-w width))
           (list v-x v-y v-w v-h v-d)
         (pcase-let
             ((`(,h-x ,h-y ,h-w ,h-h ,h-d)
                (corfu-docframe--display-area-horizontal width height)))
           (if (>= (* v-w v-h) (* h-w h-h))
               (list v-x v-y v-w v-h v-d)
             (list h-x h-y h-w h-h h-d))))))))

(defun corfu-docframe--show (&optional candidate candidate-index)
  "Show the doc popup.

The optional CANDIDATE is the completion candidate for the doc popup.

The optional CANDIDATE-INDEX is the the current completion candidate index,
it should be compared with the value recorded by `corfu--index'."
  (when (and corfu-mode
             (corfu--popup-support-p)
             (frame-live-p corfu--frame)
             (frame-visible-p corfu--frame)
             (or (null candidate-index)
                 (equal candidate-index corfu--index)))
    (when (null candidate)
      (setq candidate (and (> corfu--total 0) (>= corfu--index 0)
                           (nth corfu--index corfu--candidates))))
    (if (not candidate)
        (corfu-docframe--hide)
      (let ((should-update-doc-p
              (not (string= candidate corfu-docframe--candidate)))
            doc-updated-p
            ;; check if the coordinates of the corfu popup have changed
            (cfp-edges-changed-p
              (not (equal (frame-edges corfu--frame 'inner-edges)
                          corfu-docframe--cf-popup-edges))))
        (if (not should-update-doc-p)
            (when (and (not (string-empty-p
                             (string-trim
                              (with-current-buffer " *corfu-docframe*"
                                (buffer-string)))))
                       (not (corfu-docframe--visible-p)))
              (make-frame-visible corfu-docframe--frame))
          (if-let* ((doc (corfu-docframe--get-doc)))
              (progn
                ;; turn on word wrap and hide fringe indicators
                (with-current-buffer
                    (corfu--make-buffer " *corfu-docframe*" doc)
                  (setq-local line-move-visual t)
                  (setq-local truncate-partial-width-windows nil)
                  (setq truncate-lines nil
                        word-wrap t
                        fringe-indicator-alist `(,(cons 'continuation nil))))
                (setq doc-updated-p t))
            (corfu-docframe--hide)))
        (when (or (and (not should-update-doc-p) cfp-edges-changed-p)
                  doc-updated-p)
          (pcase-let
              ((`(,area-x ,area-y ,area-w ,area-h ,area-d)
                 (apply
                  #'corfu-docframe--display-area
                  corfu-docframe--direction
                  (when (not should-update-doc-p)
                    (let ((border
                            (alist-get 'child-frame-border-width
                                       corfu-docframe--frame-parameters)))
                      (list (- (frame-pixel-width corfu-docframe--frame)
                               border border)
                            (- (frame-pixel-height corfu-docframe--frame)
                               border border)))))))
            (setq corfu-docframe--frame
                  (corfu--make-frame corfu-docframe--frame
                                     corfu-docframe--frame-parameters
                                     area-x area-y area-w area-h
                                     (get-buffer " *corfu-docframe*"))
                  corfu-docframe--direction area-d)))
        (if doc-updated-p
            (setq corfu-docframe--candidate candidate
                  corfu-docframe--cf-popup-edges
                  (frame-edges corfu--frame 'inner-edges)
                  corfu-docframe--cf-window (selected-window))
          (when cfp-edges-changed-p
            (setq corfu-docframe--cf-popup-edges
                  (frame-edges corfu--frame 'inner-edges))))))))

(defun corfu-docframe--hide ()
  "Clear the doc popup buffer content and hide it."
  (corfu--hide-frame corfu-docframe--frame))

(defun corfu-docframe--transition ()
  "Transition when updating the doc popup."
  (when (corfu-docframe--visible-p)
    (when (and (not (null corfu-docframe-delay))
               (> corfu-docframe-delay 0))
      (if corfu-docframe-hide
          (corfu--hide-frame corfu-docframe--frame)
        (corfu-docframe--show corfu-docframe--candidate)))))

(defun corfu-docframe-scroll-up (&optional n)
  "Scroll text of doc popup window upward N lines.

If ARG is omitted or nil, scroll upward by a near full screen.
See `scroll-up' for details."
  (interactive "p")
  (when-let ((cf-doc-buf (and (corfu-docframe--visible-p)
                              (get-buffer " *corfu-docframe*"))))
    (with-selected-frame corfu-docframe--frame
      (with-current-buffer cf-doc-buf
        (scroll-up n)))))

(defun corfu-docframe-scroll-down (&optional n)
  "Scroll text of doc popup window down N lines.

If ARG is omitted or nil, scroll down by a near full screen."
  (interactive "p")
  (corfu-docframe-scroll-up (- (or n 1))))

;;;###autoload
(define-minor-mode corfu-docframe-mode
  "Corfu doc popup minor mode."
  :global nil
  :group 'corfu
  (cond
    (corfu-docframe-mode
     (corfu-docframe--show)
     (add-hook 'completion-in-region-mode-hook #'corfu-docframe--setup))
    (t
     (corfu-docframe--teardown)
     (remove-hook 'completion-in-region-mode-hook #'corfu-docframe--setup))))

(defun corfu-docframe-toggle ()
  "Toggle the doc popup display or hide.

When using this command to manually hide the doc popup, it will
not be displayed until this command is called again. Even if the
corfu doc mode is turned on and `corfu-docframe-auto' is set to Non-nil."
  (interactive)
  (when corfu-docframe-mode
    (if (corfu-docframe--visible-p)
        (progn
          (corfu-docframe--teardown)
          (remove-hook 'completion-in-region-mode-hook
                       #'corfu-docframe--setup))
      (corfu-docframe--show)
      (corfu-docframe--setup)
      (add-hook 'completion-in-region-mode-hook #'corfu-docframe--setup))))

(defun corfu-docframe--post-command ()
  "Update the doc popup after last command."
  (when corfu-preselect-first
    (advice-remove 'corfu--exhibit #'corfu-docframe--cf-exhibit-after-advice))
  (when (and (frame-live-p corfu--frame)
             (frame-visible-p corfu--frame))
    (if-let ((candidate
              (and (> corfu--total 0) (>= corfu--index 0)
                   (nth corfu--index corfu--candidates))))
        (progn
          (when corfu-docframe--auto-timer
            (cancel-timer corfu-docframe--auto-timer)
            (setq corfu-docframe--auto-timer nil))
          (if (and (string= candidate corfu-docframe--candidate)
                   (eq (selected-window) corfu-docframe--cf-window)
                   (frame-live-p corfu-docframe--frame))
              (corfu-docframe--show candidate)
            (corfu-docframe--transition)
            (setq corfu-docframe--auto-timer
                  (run-with-timer
                   corfu-docframe-delay nil
                   #'corfu-docframe--show nil corfu--index))))
      (corfu-docframe--hide))))

(defun corfu-docframe--cf-exhibit-after-advice (&rest _args)
  "After advice for `corfu--exhibit'.
To display the doc popup for the preselected completion candidate."
  (when (and corfu--candidates (>= corfu--index 0))
    (corfu-docframe--post-command)))

(defun corfu-docframe--setup ()
  "Setup corfu-docframe."
  (if (not completion-in-region-mode)
      (corfu-docframe--teardown)
    (when corfu-docframe-mode
      (if corfu-docframe-auto
          (progn
            ;; display the doc popup for the preselected first candidate
            (when corfu-preselect-first
              (advice-add 'corfu--exhibit :after
                          #'corfu-docframe--cf-exhibit-after-advice))
            (add-hook 'post-command-hook #'corfu-docframe--post-command
                      'append 'local))
        (let ((sym (make-symbol "corfu-docframe--teardown"))
              (buf (current-buffer)))
          (fset sym
                (lambda ()
                  (let ((candidate
                          (and (> corfu--total 0) (>= corfu--index 0)
                               (nth corfu--index corfu--candidates))))
                    (unless
                        (and completion-in-region-mode
                             (string= candidate corfu-docframe--candidate)
                             (eq (selected-window) corfu-docframe--cf-window)
                             (frame-live-p corfu-docframe--frame))
                      (remove-hook 'post-command-hook sym 'local)
                      (with-current-buffer (if (buffer-live-p buf)
                                               buf
                                             (current-buffer))
                        (corfu-docframe--teardown))))))
          (add-hook 'post-command-hook sym 'append 'local))))))

(defun corfu-docframe--teardown ()
  "Teardown corfu-docframe."
  (remove-hook 'post-command-hook #'corfu-docframe--post-command 'local)
  (corfu-docframe--hide)
  (mapc #'kill-local-variable corfu-docframe--state-vars))

(provide 'corfu-docframe)
;;; corfu-docframe.el ends here
