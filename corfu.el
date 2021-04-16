;;; corfu.el --- Completion Overlay Region FUnction -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler
;; Maintainer: Daniel Mendler
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Corfu provides a completion overlay for the default completion in
;; region function. The current candidates are shown in a popup
;; overlay below or above the point. Corfu can be considered the
;; minimalistic completion-in-region counterpart of Vertico.

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup corfu nil
  "Completion Overlay Region FUnction."
  :group 'convenience
  :prefix "corfu-")

(defcustom corfu-count 10
  "Maximal number of candidates to show."
  :type 'integer)

(defcustom corfu-cycle nil
  "Enable cycling for `corfu-next' and `corfu-previous'."
  :type 'boolean)

(defgroup corfu-faces nil
  "Faces used by Corfu."
  :group 'corfu
  :group 'faces)

(defface corfu-background
  '((((class color) (min-colors 88) (background dark))
     :background "#222" :inherit default)
    (((class color) (min-colors 88) (background light))
     :background "#ffe" :inherit default)
    (t :background "gray" :foreground "black" :inherit default))
  "Face used to for the overlay background.")

(defface corfu-current
  '((((class color) (min-colors 88) (background dark))
     :background "#137" :inherit default)
    (((class color) (min-colors 88) (background light))
     :background "#cef" :inherit default)
    (t :background "blue" :foreground "white" :inherit default))
  "Face used to highlight the currently selected candidate.")

(defface corfu-bar
  '((((class color) (min-colors 88) (background dark))
     :foreground "#444" :background "#bbb")
    (((class color) (min-colors 88) (background light))
     :foreground "#bbb" :background "#444")
    (t :foreground "gray" :background "black"))
  "Face used for the scrollbar.")

(defface corfu-border
  '((((class color) (min-colors 88) (background dark))
     :foreground "#444" :background "#444" )
    (((class color) (min-colors 88) (background light))
     :foreground "#bbb" :background "#bbb")
    (t :foreground "gray"))
  "Face used for the border line.")

(defvar corfu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap beginning-of-buffer] #'corfu-first)
    (define-key map [remap end-of-buffer] #'corfu-last)
    (define-key map [remap scroll-down-command] #'corfu-scroll-down)
    (define-key map [remap scroll-up-command] #'corfu-scroll-up)
    (define-key map [down] #'corfu-next)
    (define-key map [up] #'corfu-previous)
    (define-key map [remap next-line] #'corfu-next)
    (define-key map [remap previous-line] #'corfu-previous)
    (define-key map [remap completion-at-point] #'corfu-complete)
    (define-key map "\e\e\e" #'keyboard-quit)
    (define-key map "\r" #'corfu-insert)
    (define-key map "\t" #'corfu-complete)
    map)
  "Corfu keymap used when popup is shown.")

(defvar-local corfu--candidates nil
  "List of candidates.")

(defvar-local corfu--base 0
  "Size of the base string, which is concatenated with the candidate.")

(defvar-local corfu--total 0
  "Length of the candidate list `corfu--candidates'.")

(defvar-local corfu--highlight #'identity
  "Deferred candidate highlighting function.")

(defvar-local corfu--index -1
  "Index of current candidate or negative for prompt selection.")

(defvar-local corfu--input nil
  "Cons of last prompt contents and point or t.")

(defvar-local corfu--current-ov nil
  "Overlay showing the current candidate.")

(defvar-local corfu--popup-ovs nil
  "Overlay showing the candidates.")

(defvar-local corfu--extra-properties nil
  "Extra completion properties.")

(defvar-local corfu--borders nil
  "Cached border images.")

(defun corfu--char-size ()
  "Return character size in pixels."
  (let ((lh (line-pixel-height)))
    (cons (round (* lh (frame-char-width)) (frame-char-height)) lh)))

;; TODO Is there a better way to generate an image? Bitmap vector?
(defun corfu--border (w h color width)
  "Generate border with COLOR and WIDTH and image size W*H."
  (or (alist-get (cons color width) corfu--borders nil #'equal)
      (setf (alist-get (cons color width) corfu--borders nil #'equal)
            (let ((row (funcall (if (< width 0) #'reverse #'identity)
                                (concat (make-string (abs width) ?0)
                                        (make-string (- w (abs width)) ?1)))))
              (propertize
               " " 'display
               `(image :data ,(format "P1\n %s %s\n%s" w h
                                      (mapconcat (lambda (_) row) (number-sequence 1 h) ""))
                       :type pbm :scale 1 :ascent center
                       :background ,(face-attribute color :foreground)
                       :mask (heuristic (0 0 0))))))))

(defun corfu--popup (pos idx lo bar lines)
  "Show LINES as popup at POS, with IDX highlighted and scrollbar between LO and LO+BAR."
  (let* ((size (corfu--char-size))
         ;; XXX Deactivate fancy border on terminal or if line-spacing is used
         (fancy-ui (and (not line-spacing) (display-graphic-p)))
         (lborder (corfu--border (car size) (cdr size) 'corfu-border 1))
         (rborder (corfu--border (car size) (cdr size) 'corfu-border -1))
         (rbar (corfu--border (car size) (cdr size) 'corfu-bar (- (ceiling (car size) 3))))
         (col (+ (- pos (line-beginning-position)) corfu--base))
         (width (- (window-total-width) col 10))
         (pixelpos (cdr (window-absolute-pixel-position pos)))
         (row 0)
         (count (length lines))
         (tail))
    (if (< width 10)
        (setq width (/ (window-total-width) 2)
              lines (mapcar (lambda (x) (truncate-string-to-width x width)) lines)
              width (apply #'max (mapcar #'string-width lines))
              col (max 0 (- col width 2)))
      (setq lines (mapcar (lambda (x) (truncate-string-to-width x width)) lines)
            width (apply #'max (mapcar #'string-width lines))))
    (save-excursion
      (when (and (>= count (floor (- (window-pixel-height) pixelpos) (cdr size)))
                 (< count (floor pixelpos (cdr size))))
        (forward-line (- -1 count)))
      (beginning-of-line)
      (dolist (line lines)
        (let ((old (point)))
          (forward-line 1)
          (beginning-of-line)
          (when (= (point) old)
            (setq tail (concat (propertize " " 'cursor t) "\n" (make-string col 32)))))
        (let* ((beg (point))
               (end (line-end-position))
               (prefix (or tail (and (> col (- end beg)) (make-string (- col (- end beg)) 32))))
               (ov (if prefix
                       (make-overlay end end)
                     (make-overlay (min (+ beg col) end) (min (+ beg col width 2) end))))
               (str (concat
                     (if fancy-ui
                         (propertize lborder 'face (if (= row idx) 'corfu-current 'corfu-background))
                       (propertize " " 'face (if (= row idx) 'corfu-current 'corfu-background)))
                     line
                     (make-string (- width (string-width line)) 32)
                     (if fancy-ui
                         (propertize (if (and lo (<= lo row (+ lo bar))) rbar rborder)
                                     'face (if (= row idx) 'corfu-current 'corfu-background))
                       (propertize " " 'face (if (and lo (<= lo row (+ lo bar))) 'corfu-bar 'corfu-border))))))
          (add-face-text-property 0 (length str) (if (= row idx) 'corfu-current 'corfu-background) 'append str)
          (overlay-put ov 'priority (- 1000 row))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'invisible t)
          (overlay-put ov 'after-string (concat prefix str))
          (push ov corfu--popup-ovs)
          (setq row (1+ row)))))))

(defun corfu--move-to-front (elem list)
  "Move ELEM to front of LIST."
  (if-let (found (member elem list))
      (let ((head (list (car found))))
        (nconc head (delq (setcar found nil) list)))
    list))

;; bug#47711: Deferred highlighting for `completion-all-completions'
(declare-function orderless-highlight-matches "ext:orderless")
(defun corfu--all-completions (&rest args)
  "Compute all completions for ARGS with deferred highlighting."
  (cl-letf* ((orig-pcm (symbol-function #'completion-pcm--hilit-commonality))
             (orig-flex (symbol-function #'completion-flex-all-completions))
             ((symbol-function #'completion-flex-all-completions)
              (lambda (&rest args)
                ;; Unfortunately for flex we have to undo the deferred highlighting, since flex uses
                ;; the completion-score for sorting, which is applied during highlighting.
                (cl-letf (((symbol-function #'completion-pcm--hilit-commonality) orig-pcm))
                  (apply orig-flex args))))
             ;; Defer the following highlighting functions
             (hl #'identity)
             ((symbol-function #'completion-hilit-commonality)
              (lambda (cands prefix &optional base)
                (setq hl (lambda (x) (nconc (completion-hilit-commonality x prefix base) nil)))
                (and cands (nconc cands base))))
             ((symbol-function #'completion-pcm--hilit-commonality)
              (lambda (pattern cands)
                (setq hl (lambda (x) (completion-pcm--hilit-commonality pattern x)))
                cands))
             ((symbol-function #'orderless-highlight-matches)
              (lambda (pattern cands)
                (setq hl (lambda (x) (orderless-highlight-matches pattern x)))
                cands)))
    (cons (apply #'completion-all-completions args) hl)))

(defun corfu--sort-predicate (x y)
  "Sorting predicate which compares X and Y."
  (or (< (length x) (length y))
      (and (= (length x) (length y))
           (string< x y))))

(defun corfu--file-predicate (pred)
  "Filter predicate for files given original predicate PRED."
  (let ((ignore (concat "\\(?:\\`\\|/\\)\\.?\\./\\'"
                        (and completion-ignored-extensions
                             (concat "\\|" (regexp-opt completion-ignored-extensions) "\\'")))))
    (if pred
        (lambda (x) (and (not (string-match-p ignore x)) (funcall pred x)))
      (lambda (x) (not (string-match-p ignore x))))))

(defun corfu--recompute-candidates (str bounds pt table pred)
  "Recompute candidates from STR, BOUNDS, PT, TABLE and PRED."
  (let* ((field (substring str (car bounds) (+ pt (cdr bounds))))
         (metadata (completion-metadata (substring str 0 pt) table pred))
         (completing-file (eq (completion-metadata-get metadata 'category) 'file))
         (all-hl (corfu--all-completions str table
                                         (if completing-file
                                             (corfu--file-predicate pred)
                                           pred)
                                         pt metadata))
         (all (car all-hl))
         (base (if-let (last (last all)) (prog1 (cdr last) (setcdr last nil)) 0)))
    (setq all (if-let (sort (completion-metadata-get metadata 'display-sort-function))
                  (funcall sort all)
                (sort all #'corfu--sort-predicate)))
    ;; Move candidates which match prefix to the beginning
    (let* ((word (replace-regexp-in-string " .*" "" field))
           (prefix (seq-filter (lambda (x) (string-prefix-p word x)) all))
           (not-prefix (seq-remove (lambda (x) (string-prefix-p word x)) all)))
      (setq all (nconc prefix not-prefix)))
    (when (and completing-file (not (string-suffix-p "/" field)))
      (setq all (corfu--move-to-front (concat field "/") all)))
    (setq all (corfu--move-to-front field all))
    (list base (length all) all (cdr all-hl))))

(defun corfu--update-candidates (str bounds pt table pred)
  "Update candidates from STR, BOUNDS, PT, TABLE and PRED."
  (pcase (let ((while-no-input-ignore-events '(selection-request)))
           (while-no-input (corfu--recompute-candidates str bounds pt table pred)))
    (`(,base ,total ,candidates ,hl)
     (setq corfu--input (cons str pt)
           corfu--candidates candidates
           corfu--base base
           corfu--total total
           corfu--highlight hl))))

(defun corfu--pre-command-hook ()
  "Delete overlays."
  (mapc #'delete-overlay corfu--popup-ovs)
  (setq corfu--popup-ovs nil)
  (when (and (>= corfu--index 0)
             (not (string-prefix-p "corfu-" (prin1-to-string this-command)))
             (not (eq this-command 'keyboard-quit)))
    (corfu-insert)))

(defun corfu--refresh (beg end table pred)
  "Refresh Corfu overlays, given BEG, END, TABLE and PRED."
  (let* ((pt (- (point) beg))
         (str (buffer-substring-no-properties beg end))
         (before (substring str 0 pt))
         (after (substring str pt))
         ;; bug#47678: `completion-boundaries` fails for `partial-completion`
         ;; if the cursor is moved between the slashes of "~//".
         ;; See also vertico.el which has the same issue.
         (bounds (or (condition-case nil
                         (completion-boundaries before
                                                table
                                                pred
                                                after)
                       (t (cons 0 (length after)))))))
    (unless (equal corfu--input (cons str pt))
      (corfu--update-candidates str bounds pt table pred))
    (when (and
           ;; Empty input
           (or (eq this-command 'completion-at-point)
               (string-prefix-p "corfu-" (prin1-to-string this-command))
               (/= beg end))
           ;; Input after boundary is empty
           (not (and (= (car bounds) (length str))
                     (test-completion str table pred)))
           ;; No candidates
           corfu--candidates
           ;; Single candidate
           (not (equal corfu--candidates (list str))))
      (let* ((start (min (max 0 (- corfu--index (/ corfu-count 2)))
                         (max 0 (- corfu--total corfu-count))))
             (end (min (+ start corfu-count) corfu--total))
             (bar (ceiling (* corfu-count corfu-count) corfu--total))
             (lo (min (- corfu-count bar 1) (floor (* corfu-count start) corfu--total))))
        ;; Nonlinearity at the end and the beginning
        (when (/= start 0)
          (setq lo (max 1 lo)))
        (when (/= end corfu--total)
          (setq lo (min (- corfu-count bar 2) lo)))
        (corfu--popup beg
                      (- corfu--index start)
                      (and (> corfu--total corfu-count) lo)
                      bar
                      (funcall corfu--highlight
                               (seq-subseq corfu--candidates start end)))))))

(defun corfu--post-command-hook ()
  "Refresh Corfu after last command."
  (pcase completion-in-region--data
    ((and `(,beg ,end ,table ,pred)
          (guard (eq (marker-buffer beg) (current-buffer)))
          (guard (<= beg (point) end)))
     (corfu--refresh beg end table pred)))
  (unless corfu--popup-ovs
    (completion-in-region-mode -1)))

(defun corfu--goto (index)
  "Go to candidate with INDEX."
  (setq corfu--index (max -1 (min index (- corfu--total 1))))
  (if (< corfu--index 0)
      (when corfu--current-ov
        (delete-overlay corfu--current-ov)
        (setq corfu--current-ov nil))
    (pcase-let ((`(,beg ,end . ,_) completion-in-region--data))
      (unless corfu--current-ov
        (setq corfu--current-ov (make-overlay beg end nil t t))
        (overlay-put corfu--current-ov 'priority 1000)
        (overlay-put corfu--current-ov 'window (selected-window)))
      (overlay-put corfu--current-ov 'display (nth corfu--index corfu--candidates)))))

(defun corfu-next ()
  "Go to next candidate."
  (interactive)
  (corfu--goto
   (if (and corfu-cycle (= (1+ corfu--index) corfu--total))
       -1
     (1+ corfu--index))))

(defun corfu-previous ()
  "Go to previous candidate."
  (interactive)
  (corfu--goto
   (if (and corfu-cycle (< corfu--index 0))
       (- corfu--total 1)
     (- corfu--index 1))))

(defun corfu-scroll-down ()
  "Go back by one page."
  (interactive)
  (corfu--goto (max 0 (- corfu--index corfu-count))))

(defun corfu-scroll-up ()
  "Go forward by one page."
  (interactive)
  (corfu--goto (+ corfu--index corfu-count)))

(defun corfu-first ()
  "Go to first candidate."
  (interactive)
  (corfu--goto 0))

(defun corfu-last ()
  "Go to last candidate."
  (interactive)
  (corfu--goto (- corfu--total 1)))

(defun corfu-complete ()
  "Try to complete current input."
  (interactive)
  (if (>= corfu--index 0)
      (corfu-insert)
    (pcase-let* ((`(,beg ,end ,table ,pred) completion-in-region--data)
                 (pt (max 0 (- (point) beg)))
                 (str (buffer-substring-no-properties beg end))
                 (metadata (completion-metadata (substring str 0 pt) table pred)))
      (pcase (completion-try-completion str table pred pt metadata)
        ((and `(,newstr . ,newpt) (guard (not (equal str newstr))))
         (completion--replace beg end newstr)
         (goto-char (+ beg newpt)))))))

(defun corfu-insert ()
  "Insert current candidate."
  (interactive)
  (pcase-let* ((`(,beg ,end . _) completion-in-region--data)
               (str (buffer-substring-no-properties beg end))
               (newstr (concat (substring str 0 corfu--base)
                               (substring-no-properties (nth (max 0 corfu--index) corfu--candidates)))))
    (completion--replace beg end newstr)
    (when-let (exit (plist-get corfu--extra-properties :exit-function))
      (funcall exit newstr 'finished))
    (completion-in-region-mode -1)))

(defun corfu--setup ()
  "Setup Corfu completion state."
  ;; Keep completion alive when popup is shown (disable predicate check!)
  (remove-hook 'post-command-hook #'completion-in-region--postch)
  (set (make-local-variable 'completion-show-inline-help) nil)
  (set (make-local-variable 'completion-auto-help) nil)
  (setq corfu--extra-properties completion-extra-properties)
  (setcdr (assq #'completion-in-region-mode minor-mode-overriding-map-alist) corfu-map)
  (add-hook 'pre-command-hook #'corfu--pre-command-hook nil 'local)
  (add-hook 'post-command-hook #'corfu--post-command-hook nil 'local))

(defun corfu--teardown ()
  "Teardown Corfu."
  (mapc #'delete-overlay corfu--popup-ovs)
  (when corfu--current-ov (delete-overlay corfu--current-ov))
  (remove-hook 'pre-command-hook #'corfu--pre-command-hook 'local)
  (remove-hook 'post-command-hook #'corfu--post-command-hook 'local)
  (mapc #'kill-local-variable '(corfu--base
                                corfu--candidates
                                corfu--highlight
                                corfu--index
                                corfu--input
                                corfu--total
                                corfu--popup-ovs
                                corfu--current-ov
                                corfu--borders
                                corfu--extra-properties
                                completion-show-inline-help
                                completion-auto-help)))

(defun corfu--mode-hook ()
  "Corfu mode hook."
  (if completion-in-region-mode
      (corfu--setup)
    (corfu--teardown)))

;;;###autoload
(define-minor-mode corfu-mode
  "Completion Overlay Region FUnction"
  :local t
  (if corfu-mode
      (add-hook 'completion-in-region-mode-hook #'corfu--mode-hook nil 'local)
    (remove-hook 'completion-in-region-mode-hook #'corfu--mode-hook 'local)))

(provide 'corfu)
;;; corfu.el ends here
