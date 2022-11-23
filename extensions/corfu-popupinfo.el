;;; corfu-popupinfo.el --- Candidate information popup for Corfu -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

;; Author: Yuwei Tian <fishtai0@gmail.com>, Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (corfu "0.33"))
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
;; Display an information popup for completion candidate when using
;; Corfu. The popup displays either the candidate documentation or the
;; candidate location. The `corfu-popupinfo-mode' must be enabled
;; globally. Set `corfu-popupinfo-delay' to nil if the info popup should
;; not update automatically. If the popup should not appear initially,
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
  '((t :inherit corfu-default :height 0.8))
  "Face used for the info popup."
  :group 'corfu-faces)

(defcustom corfu-popupinfo-delay '(2.0 . 1.0)
  "Automatically update info popup after that number of seconds.

Set to t for an instant update. The value can be a pair of two
floats to specify initial and subsequent delay. If the value is
non-nil or the car of the pair is non-nil, the popup will
automatically appear for the preselected candidate. Otherwise the
popup can be requested manually via `corfu-popupinfo-toggle',
`corfu-popupinfo-documentation' and `corfu-popupinfo-location'."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Instant" t)
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
  "The max width of the info popup in characters."
  :type 'integer
  :group 'corfu)

(defcustom corfu-popupinfo-max-height 10
  "The max height of the info popup in characters."
  :type 'integer
  :group 'corfu)

(defcustom corfu-popupinfo-resize t
  "Resize the info popup automatically if non-nil."
  :type 'boolean
  :group 'corfu)

;; TODO Not yet fully supported.
(defcustom corfu-popupinfo-direction 'horizontal
  "Preferred direction for the popup."
  :type '(choice (const horizontal)
                 (const vertical)
                 ;; TODO always-* are unsupported
                 (const always-horizontal)
                 (const always-vertical)
                 (const always-left)
                 (const always-right))
  :group 'corfu)

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
all values are in pixels relative to the origin. See
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
          (when-let* ((fun (plist-get corfu--extra :company-location))
                      ;; BUG: company-location may throw errors if location is not found
                      (loc (ignore-errors (funcall fun candidate)))
                      ((setq buffer
                             (or (and (bufferp (car loc)) (car loc))
                                 (get-file-buffer (car loc))
                                 (let ((inhibit-message t)
                                       (enable-dir-local-variables nil)
                                       (enable-local-variables :safe)
                                       (non-essential t)
                                       (delay-mode-hooks t)
                                       (find-file-hook '(global-font-lock-mode-check-buffers)))
                                   (find-file-noselect (car loc) t))))))
            (with-current-buffer buffer
              (save-excursion
                (save-restriction
                  (widen)
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
      (setq res (string-trim
                 (replace-regexp-in-string
                  "[\\s-\n]*\\[back\\][\\s-\n]*" ""
                  (buffer-string))))
      (and (not (string-blank-p res)) res))))

(defun corfu-popupinfo--size ()
  "Return popup size as pair."
  (let* ((cw (default-font-width))
         (margin (* cw (+ (alist-get 'left-margin-width corfu-popupinfo--buffer-parameters)
                          (alist-get 'right-margin-width corfu-popupinfo--buffer-parameters))))
         (max-height (* (default-line-height) corfu-popupinfo-max-height))
         (max-width (* cw corfu-popupinfo-max-width)))
    (or (when corfu-popupinfo-resize
          (with-current-buffer " *corfu-popupinfo*"
            (cl-letf* (((window-dedicated-p) nil)
                       ((window-buffer) (current-buffer))
                       (size (window-text-pixel-size
                              nil (point-min) (point-max)
                              max-width max-height)))
              ;; Check that width is not exceeded. Otherwise use full height,
              ;; since lines will get wrapped.
              (when (<= (car size) max-width)
                (cons (+ margin (car size)) (min (cdr size) max-height))))))
        (cons (+ margin max-width) max-height))))

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

The calculated area is in the form (X Y WIDTH HEIGHT 'vertical)."
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
                ;;height (min height (* (floor (/ height lh)) lh))
                )
          (list cfx y-on-bottom w-avail height 'vertical))
      (setq height (min h-remaining-top height)
            ;;height (min height (* (floor (/ height lh)) lh))
            )
      (list cfx
            (max 0 (- cfy height border))
            w-avail height 'vertical))))

(defun corfu-popupinfo--display-area (dir width height)
  "Calculate the display area for the info popup.

If DIR is non-nil, the display area in the corresponding
direction is calculated first, its value can be 'vertical, 'right
or 'left.

The pixel size of the info popup can be specified by WIDTH and HEIGHT.

The calculated area is in the form (X Y WIDTH HEIGHT DIR).
DIR indicates the position direction of the info popup relative to
the candidate popup, its value is 'vertical, 'right or 'left."
  (unless (and width height)
    (let ((size (corfu-popupinfo--size)))
      (setq width (car size)
            height (cdr size))))
  (cond
   ((or (eq dir 'right) (eq dir 'left))
    ;; TODO Direction handling is incomplete. Fix not only horizontal,
    ;; but also left or right.
    (corfu-popupinfo--display-area-horizontal width height))
   ((eq dir 'vertical)
    (corfu-popupinfo--display-area-vertical width height))
   (t
    (pcase-let* (((and h-a `(,_h-x ,_h-y ,h-w ,h-h ,_h-d))
                  (corfu-popupinfo--display-area-horizontal width height))
                 ((and v-a `(,_v-x ,_v-y ,v-w ,v-h ,_v-d))
                  (corfu-popupinfo--display-area-vertical width height)))
      (pcase corfu-popupinfo-direction
        ;; TODO Add proper support for corfu-popupinfo-direction 'always-left,
        ;; 'always-right.
        ('always-left       h-a)
        ('always-right      h-a)
        ('always-horizontal h-a)
        ('always-vertical   v-a)
        ((and 'horizontal (guard (>= h-h height)) (guard (>= h-w width))) h-a)
        ((and 'vertical   (guard (>= v-h height)) (guard (>= v-w width))) v-a)
        (_ (if (>= (* v-w v-h) (* h-w h-h)) v-a h-a)))))))

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
            (with-current-buffer (corfu--make-buffer " *corfu-popupinfo*" content)
              ;; TODO Could we somehow refill the buffer intelligently?
              ;;(let ((inhibit-read-only t))
              ;;  (setq fill-column corfu-popupinfo-max-width)
              ;;  (fill-region (point-min) (point-max)))
              (dolist (var corfu-popupinfo--buffer-parameters)
                (set (make-local-variable (car var)) (cdr var)))
              (setf face-remapping-alist (copy-tree face-remapping-alist)
                    (alist-get 'default face-remapping-alist) 'corfu-popupinfo))
          (unless (eq corfu-popupinfo--toggle 'init)
            (message "No %s available for `%s'"
                     (car (last (split-string (symbol-name corfu-popupinfo--function) "-+")))
                     candidate))
          (corfu-popupinfo--hide)
          (setq cand-changed nil coords-changed nil)))
      (when (or cand-changed coords-changed)
        (pcase-let* ((border (alist-get 'child-frame-border-width corfu--frame-parameters))
                     (`(,area-x ,area-y ,area-w ,area-h ,area-d)
                      (corfu-popupinfo--display-area
                       corfu-popupinfo--lock-dir
                       (and (not cand-changed)
                            (- (frame-pixel-width corfu-popupinfo--frame) border border))
                       (and (not cand-changed)
                            (- (frame-pixel-height corfu-popupinfo--frame) border border))))
                     (margin-quirk (not corfu-popupinfo--frame)))
          (setq corfu-popupinfo--frame
                (corfu--make-frame corfu-popupinfo--frame
                                   area-x area-y area-w area-h
                                   " *corfu-popupinfo*")
                corfu-popupinfo--toggle t
                corfu-popupinfo--lock-dir area-d
                corfu-popupinfo--candidate candidate
                corfu-popupinfo--coordinates new-coords)
          ;; HACK: Force margin update. For some reason, the call to
          ;; `set-window-buffer' in `corfu--make-frame' is not effective the
          ;; first time. Why does Emacs have all these quirks?
          (when margin-quirk
            (set-window-buffer
             (frame-root-window corfu-popupinfo--frame)
             " *corfu-popupinfo*")))))))

(defun corfu-popupinfo--hide ()
  "Clear the info popup buffer content and hide it."
  (corfu--hide-frame corfu-popupinfo--frame))

(defun corfu-popupinfo-scroll-up (&optional n)
  "Scroll text of info popup window upward N lines.

If ARG is omitted or nil, scroll upward by a near full screen.
See `scroll-up' for details. If the info popup is not visible,
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
    (corfu-popupinfo--show (nth corfu--index corfu--candidates))))

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

(defun corfu-popupinfo--exhibit (&rest _)
  "Update the info popup automatically."
  (when completion-in-region-mode
    (setf (alist-get #'corfu-popupinfo-mode minor-mode-overriding-map-alist)
          corfu-popupinfo-map)
    (when corfu-popupinfo--timer
      (cancel-timer corfu-popupinfo--timer)
      (setq corfu-popupinfo--timer nil))
    (if (and (>= corfu--index 0) (corfu-popupinfo--visible-p corfu--frame))
        (when-let* ((delay (if (consp corfu-popupinfo-delay)
                               (funcall (if (eq corfu-popupinfo--toggle 'init) #'car #'cdr)
                                        corfu-popupinfo-delay)
                             corfu-popupinfo-delay))
                    (corfu-popupinfo--toggle))
          (let ((candidate (nth corfu--index corfu--candidates)))
            (if (or (eq delay t) (<= delay 0)
                    (and (equal candidate corfu-popupinfo--candidate)
                         (corfu-popupinfo--visible-p)))
                (corfu-popupinfo--show candidate)
              (when (corfu-popupinfo--visible-p)
                (cond
                  (corfu-popupinfo-hide
                   (corfu-popupinfo--hide))
                  (corfu-popupinfo--candidate
                   (corfu-popupinfo--show corfu-popupinfo--candidate))))
              (setq corfu-popupinfo--timer
                    (run-at-time delay nil #'corfu-popupinfo--show candidate)))))
      (corfu-popupinfo--hide))))

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
