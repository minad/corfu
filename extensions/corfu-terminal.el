;;; corfu-terminal.el --- Corfu popup on terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-04-11
;; Version: 0.5
;; Package-Requires: ((emacs "26.1") (corfu "0.35"))
;; Keywords: convenience
;; Homepage: https://codeberg.org/akib/emacs-corfu-terminal

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Corfu uses child frames to display candidates.  This makes Corfu
;; unusable on terminal.  This package replaces that with popup/popon,
;; which works everywhere.  Use M-x corfu-terminal-mode to enable.
;; You'll probably want to enable it only on terminal.  In that case,
;; put the following in your init file:

;;   (unless (display-graphic-p)
;;     (corfu-terminal-mode +1))

;;; Code:

(require 'subr-x)
(require 'corfu)
(require 'cl-lib)

(defgroup corfu-terminal nil
  "Corfu popup on terminal."
  :group 'convenience
  :link '(url-link "https://codeberg.org/akib/emacs-corfu-terminal")
  :prefix "corfu-terminal-")

(defvar-local corfu-terminal--overlays nil
  "Overlays object.")

(cl-defmethod corfu--popup-support-p (&context (corfu-terminal-mode (eql t)))
  "Return whether corfu-terminal supports showing popon now."
  t)

(cl-defmethod corfu--popup-hide (&context (corfu-terminal-mode (eql t)))
  "Hide popup."
  (while corfu-terminal--overlays
    (delete-overlay (pop corfu-terminal--overlays))))

(cl-defmethod corfu--popup-show (pos off width lines
                                     &context (corfu-terminal-mode (eql t))
                                     &optional curr lo bar)
  "Show popup at OFF columns before POS.

Show LINES, a list of lines.  Highlight CURRth line as current
selection.  Show a vertical scroll bar of size BAR + 1 from LOth line."
  (corfu--popup-hide) ; Hide the popup first.
  (let* ((bar-width 1)
         (scroll-bar (propertize " " 'face 'corfu-bar))
         (margin-right (make-string bar-width ?\ ))
         (popon-width (+ width bar-width))
         (pos (corfu-terminal--x-y-at-pos (if (posnp pos) (posn-point pos) pos)))
         (popon-pos
          (let ((x (max 0 (min (- (car pos) off)
                               (- (window-max-chars-per-line) popon-width))))
                (y (if (and (<= (window-body-height) (+ (cdr pos) (length lines)))
                            (>= (cdr pos) (length lines)))
                       (- (cdr pos) (length lines))
                     (1+ (cdr pos)))))
            (cons x y))))
    (corfu-terminal--render
     (string-join
      (seq-map-indexed
       (lambda (line line-number)
         (let* ((pad (make-string (- width (string-width line)) ?\ ))
                (bar (if (and lo (<= lo line-number (+ lo bar)))
                         scroll-bar margin-right))
                (str (concat line pad bar))
                (face (if (eq line-number curr)
                          'corfu-current 'corfu-default)))
           (add-face-text-property 0 (length str) face t str)
           str))
       lines)
      "\n")
     popon-width
     popon-pos)))

(defun corfu-terminal--x-y-at-pos (point)
  (let ((window-start-x-y (posn-col-row (posn-at-point (window-start))))
        (point-x-y (posn-col-row (posn-at-point point))))
    (cons (if (> (window-hscroll) 0)
              (- (save-excursion (goto-char point) (current-column))
                 (window-hscroll))
            (- (car point-x-y) (car window-start-x-y)))
          (- (cdr point-x-y) (cdr window-start-x-y)))))

(defun corfu-terminal--is-invisible (point)
  (get-char-property point 'invisible))

(defun corfu-terminal--next-invisible (point &optional end)
  (next-single-char-property-change point 'invisible nil (or end (window-end))))

(defun corfu-terminal--buffer-visible-substring (start end)
  (let ((str nil))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((next-change (corfu-terminal--next-invisible (point) end)))
          (if (corfu-terminal--is-invisible (point))
              (push "..." str)
            (push (buffer-substring (point) next-change) str))
          (goto-char next-change))))
    (string-join (reverse str))))

(defun corfu-terminal--make-framebuffer ()
  "Create a framebuffer for current window and buffer."
  (let ((framebuffer nil))
    (save-excursion
      (goto-char (window-start))
      (let ((mark (point))
            (next-invisible
             (if (corfu-terminal--is-invisible (point))
                 (point)
               (corfu-terminal--next-invisible (point))))
            (point-to-line nil))
        (dotimes (i (floor (window-screen-lines)))
          (if truncate-lines (forward-line 1) (vertical-motion 1))
          (when (< next-invisible (point))
            (let ((next-visible (corfu-terminal--next-invisible next-invisible)))
              (setq next-invisible (corfu-terminal--next-invisible next-visible))
              (while (> next-visible (point))
                (if truncate-lines (forward-line 1) (vertical-motion 1)))))
          (let* ((str (corfu-terminal--buffer-visible-substring mark (point)))
                 (disp-str (string-trim-right str "\n"))
                 (end (if (equal str disp-str) (point) (1- (point))))
                 (line (alist-get mark point-to-line)))
            (unless line
              (setq line i)
              (setf (alist-get mark point-to-line) line))
            ;; (str mod ext line start end)
            (push (list disp-str nil nil line mark end) framebuffer))
          (setq mark (point)))))
    (nreverse framebuffer)))

(defun corfu-terminal--render-lines (framebuffer x y lines width)
  (let ((tab-size tab-width)
        ;; The text might have `read-only' property.
        (inhibit-read-only t))
    (with-temp-buffer
      (setq-local tab-width tab-size) ; Preseve tab width.
      (dotimes (i (length lines))
        (when (< (+ y i) (length framebuffer))
          (erase-buffer)
          (insert (car (nth (+ y i) framebuffer)))
          (goto-char (point-min))
          (let ((end (line-end-position)))
            (move-to-column x t)
            (let ((mark (point)))
              (move-to-column (+ x width) t)
              (setf (car (nth (+ y i) framebuffer))
                    (concat (buffer-substring (point-min) mark)
                            (nth i lines)
                            (buffer-substring (point) (point-max))))
              (setf (cadr (nth (+ y i) framebuffer)) t)
              (when (< end (point-max))
                (setf (cadr (cdr (nth (+ y i) framebuffer))) t))))))))
  framebuffer)

(defun corfu-terminal--make-overlays (framebuffer)
  "Make overlays to display FRAMEBUFFER on window."
  (let ((line-map nil))
    (seq-map
     (lambda (line)
       (when (nth 1 line) ;; modified
         (let* ((key (cons (nth 4 line) (nth 5 line)))
                (block (assoc key line-map)))
           (unless block
             (setq block (list key nil))
             (push block line-map))
           (push (car line) (cadr block)))))
     framebuffer)
    (dolist (block line-map)
      (let ((ov (make-overlay (caar block) (cdar block))))
        (push ov corfu-terminal--overlays)
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'line-prefix "")
        (overlay-put ov 'wrap-prefix "")
        (overlay-put ov 'display (copy-sequence '(space :width (0))))
        (overlay-put
         ov 'before-string
         (string-join (reverse (cadr block)) "\n"))))))

(defun corfu-terminal--render (text width pos)
  (while corfu-terminal--overlays
    (delete-overlay (pop corfu-terminal--overlays)))
  (let ((lines (split-string text "\n"))
        (x (+ (car pos) (window-hscroll)))
        (y (cdr pos))
        (framebuffer (corfu-terminal--make-framebuffer)))
    (corfu-terminal--render-lines framebuffer x y lines width)
    (corfu-terminal--make-overlays framebuffer)))

;;;###autoload
(define-minor-mode corfu-terminal-mode
  "Corfu popup on terminal."
  :global t
  :group 'corfu-terminal)

(provide 'corfu-terminal)
;;; corfu-terminal.el ends here
