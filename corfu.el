;;; corfu.el --- Completion Overlay Region FUnction -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
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

(defcustom corfu-min-width 15
  "Minimum popup width."
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
    (define-key map "\e\e\e" #'corfu-abort)
    (define-key map "\C-g" #'corfu-abort)
    (define-key map "\r" #'corfu-insert)
    (define-key map "\t" #'corfu-complete)
    (define-key map "\eg" #'corfu-show-location)
    (define-key map "\eh" #'corfu-show-documentation)
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

(defvar-local corfu--overlays nil
  "Overlay showing the candidates.")

(defvar-local corfu--extra-properties nil
  "Extra completion properties.")

(defvar corfu--keep-alive
  "\\`\\(corfu-\\|scroll-other-window\\)"
  "Keep Corfu popup alive during commands matching this regexp.")

(defconst corfu--state-vars
  '(corfu--base
    corfu--candidates
    corfu--highlight
    corfu--index
    corfu--input
    corfu--total
    corfu--overlays
    corfu--extra-properties)
  "Buffer-local state variables used by Corfu.")

(defun corfu--char-size ()
  "Return character size in pixels."
  (let ((lh (line-pixel-height)))
    (cons (round (* lh (frame-char-width)) (frame-char-height)) lh)))

;; XXX Is there a better way to generate an image? Bitmap vector?
(defun corfu--border (w h color width)
  "Generate border with COLOR and WIDTH and image size W*H."
  (let ((row (if (< width 0)
                 (concat (make-string (- w (- width)) ?1) (make-string (- width) ?0))
               (concat (make-string width ?0) (make-string (- w width) ?1)))))
    (propertize
     " " 'display
     `(image :data ,(format "P1\n%s %s\n%s" w h
                            (mapconcat (lambda (_) row) (number-sequence 1 h) ""))
             :type pbm :scale 1 :ascent center
             :background ,(face-attribute color :foreground)
             :mask (heuristic (0 0 0))))))

(defun corfu--popup (pos idx lo bar lines)
  "Show LINES as popup at POS, with IDX highlighted and scrollbar between LO and LO+BAR."
  (let* ((size (corfu--char-size))
         ;; XXX Deactivate fancy border on terminal or if line-spacing is used
         (fancy-ui (and (not line-spacing) (display-graphic-p)))
         (lborder (corfu--border (car size) (cdr size) 'corfu-border 1))
         (rborder (corfu--border (car size) (cdr size) 'corfu-border -1))
         (rbar (corfu--border (car size) (cdr size) 'corfu-bar (- (ceiling (car size) 3))))
         (col (+ (- pos (line-beginning-position)) corfu--base))
         (max-width (min (/ (window-total-width) 2) (- (window-total-width) col 4)))
         (ypos (- (line-number-at-pos pos)
                  (save-excursion (move-to-window-line 0) (line-number-at-pos))))
         (count (length lines))
         (row 0) (width) (formatted) (beg))
    (if (< max-width corfu-min-width)
        (setq width (max corfu-min-width (/ (window-total-width) 2))
              lines (mapcar (lambda (x) (truncate-string-to-width x width)) lines)
              width (apply #'max (mapcar #'string-width lines))
              col (max 0 (- col width 2)))
      (setq lines (mapcar (lambda (x) (truncate-string-to-width x max-width)) lines)
            width (apply #'max corfu-min-width (mapcar #'string-width lines))))
    (save-excursion
      (beginning-of-line)
      (forward-line (if (and (< count ypos)
                             (>= count (- (floor (window-pixel-height) (cdr size)) ypos 1)))
                        (- count) 1))
      (setq beg (point))
      (when (save-excursion
              (forward-line 1)
              (/= (point) (line-beginning-position)))
        (push #(" \n" 0 1 (cursor t)) formatted))
      (dolist (line lines)
        (let ((bufline (buffer-substring (point) (line-end-position)))
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
          (push (concat
                 (truncate-string-to-width bufline col 0 32) str
                 (substring bufline (length (truncate-string-to-width bufline (+ col width 2))))
                 "\n")
                formatted)
          (setq row (1+ row))
          (forward-line 1)))
      (let ((ov (make-overlay beg (point))))
        (overlay-put ov 'priority 900)
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'invisible t)
        (overlay-put ov 'before-string (string-join (nreverse formatted)))
        (push ov corfu--overlays)))))

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

(defun corfu--recompute-candidates (str bounds metadata pt table pred)
  "Recompute candidates from STR, BOUNDS, METADATA, PT, TABLE and PRED."
  (let* ((field (substring str (car bounds) (+ pt (cdr bounds))))
         (completing-file (eq (corfu--metadata-get metadata 'category) 'file))
         (all-hl (corfu--all-completions str table
                                         (if completing-file
                                             (corfu--file-predicate pred)
                                           pred)
                                         pt metadata))
         (all (car all-hl))
         (base (if-let (last (last all)) (prog1 (cdr last) (setcdr last nil)) 0)))
    (setq all (if-let (sort (corfu--metadata-get metadata 'display-sort-function))
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

(defun corfu--update-candidates (str bounds metadata pt table pred)
  "Update candidates from STR, BOUNDS, METADATA, PT, TABLE and PRED."
  (pcase (let ((while-no-input-ignore-events '(selection-request)))
           (while-no-input (corfu--recompute-candidates str bounds metadata pt table pred)))
    (`(,base ,total ,candidates ,hl)
     (setq corfu--input (cons str pt)
           corfu--candidates candidates
           corfu--base base
           corfu--total total
           corfu--highlight hl))))

(defun corfu--pre-command-hook ()
  "Delete overlays."
  (mapc #'delete-overlay corfu--overlays)
  (setq corfu--overlays nil)
  (unless (or (< corfu--index 0)
              (string-match-p corfu--keep-alive (prin1-to-string this-command)))
    (corfu-insert)))

(defun corfu-abort ()
  "Abort Corfu completion."
  (interactive)
  (completion-in-region-mode -1))

(defun corfu--annotate (metadata candidates)
  "Annotate CANDIDATES with annotation function specified by METADATA."
  (if-let (aff (or (corfu--metadata-get metadata 'affixation-function)
                   (plist-get corfu--extra-properties :affixation-function)))
      (funcall aff candidates)
    (if-let (ann (or (corfu--metadata-get metadata 'annotation-function)
                     (plist-get corfu--extra-properties :annotation-function)))
        (mapcar (lambda (cand) (list cand (or (funcall ann cand) ""))) candidates)
      candidates)))

;; XXX Do not use `completion-metadata-get' in order to avoid Marginalia.
;; The Marginalia annotators are way to heavy for the Corfu popup!
(defun corfu--metadata-get (metadata prop)
  "Return PROP from METADATA."
  (cdr (assq prop metadata)))

(defun corfu--format-candidate (ann-cand)
  "Format annotated ANN-CAND string."
  (let* ((prefix "") (suffix "")
         (cand (pcase ann-cand
                 (`(,c ,s) (setq suffix s) c)
                 (`(,c ,p ,s) (setq prefix p suffix s) c)
                 (c c))))
    (concat prefix cand
            (if (text-property-not-all 0 (length suffix) 'face nil suffix)
                suffix
              (propertize suffix 'face 'completions-annotations)))))

(defun corfu--update-display ()
  "Refresh Corfu UI."
  (pcase-let* ((`(,beg ,end ,table ,pred) completion-in-region--data)
               (pt (- (point) beg))
               (str (buffer-substring-no-properties beg end))
               (metadata (completion-metadata (substring str 0 pt) table pred))
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
      (corfu--update-candidates str bounds metadata pt table pred))
    (when (and
           ;; Empty input
           (or (eq this-command 'completion-at-point)
               (string-match-p corfu--keep-alive (prin1-to-string this-command))
               (/= beg end))
           ;; Input after boundary is empty
           (not (and (= (car bounds) (length str)) (test-completion str table pred)))
           ;; XXX Completion is terminated if there are no matches. Add optional confirmation?
           corfu--candidates
           ;; Single candidate
           (not (equal corfu--candidates (list str))))
      (let* ((start (min (max 0 (- corfu--index (/ corfu-count 2)))
                         (max 0 (- corfu--total corfu-count))))
             (curr (- corfu--index start))
             (last (min (+ start corfu-count) corfu--total))
             (bar (ceiling (* corfu-count corfu-count) corfu--total))
             (lo (min (- corfu-count bar 1) (floor (* corfu-count start) corfu--total)))
             (cands (funcall corfu--highlight (seq-subseq corfu--candidates start last)))
             (ann-cands (mapcar #'corfu--format-candidate (corfu--annotate metadata cands))))
        (when (>= curr 0)
          (let ((ov (make-overlay beg end nil t t)))
            (overlay-put ov 'priority 1000)
            (overlay-put ov 'window (selected-window))
            (overlay-put ov 'display (nth curr cands))
            (push ov corfu--overlays)))
        ;; Nonlinearity at the end and the beginning
        (when (/= start 0)
          (setq lo (max 1 lo)))
        (when (/= last corfu--total)
          (setq lo (min (- corfu-count bar 2) lo)))
        (corfu--popup beg curr (and (> corfu--total corfu-count) lo) bar ann-cands)))))

(defun corfu--post-command-hook ()
  "Refresh Corfu after last command."
  (pcase completion-in-region--data
    (`(,beg ,end ,_table ,_pred)
     (when (and (eq (marker-buffer beg) (current-buffer)) (<= beg (point) end))
       (corfu--update-display))))
  (unless corfu--overlays
    (completion-in-region-mode -1)))

(defun corfu--goto (index)
  "Go to candidate with INDEX."
  (setq corfu--index (max -1 (min index (- corfu--total 1)))))

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

(defun corfu--restore-on-next-command ()
  "Restore window configuration before next command."
  (let ((config (current-window-configuration))
        (other other-window-scroll-buffer)
        (restore (make-symbol "corfu--restore")))
    (fset restore (lambda ()
                 (when (eq this-command #'corfu-abort)
                   (setq this-command #'ignore))
                 (remove-hook 'pre-command-hook restore)
                 (setq other-window-scroll-buffer other)
                 (set-window-configuration config)))
    (run-at-time 0 nil (lambda () (add-hook 'pre-command-hook restore)))))

;; Company support, taken from `company.el', see `company-show-doc-buffer'.
(defun corfu-show-documentation ()
  "Show documentation of current candidate."
  (interactive)
  (when (< corfu--index 0)
    (user-error "No candidate selected"))
  (if-let* ((fun (plist-get corfu--extra-properties :company-doc-buffer))
            (res (funcall fun (nth corfu--index corfu--candidates))))
      (let ((buf (or (car-safe res) res)))
        (corfu--restore-on-next-command)
        (setq other-window-scroll-buffer (get-buffer buf))
        (set-window-start (display-buffer buf t) (or (cdr-safe res) (point-min))))
    (user-error "No documentation available")))

;; Company support, taken from `company.el', see `company-show-location'.
(defun corfu-show-location ()
  "Show location of current candidate."
  (interactive)
  (when (< corfu--index 0)
    (user-error "No candidate selected"))
  (if-let* ((fun (plist-get corfu--extra-properties :company-location))
            (loc (funcall fun (nth corfu--index corfu--candidates))))
      (let ((buf (or (and (bufferp (car loc)) (car loc)) (find-file-noselect (car loc) t))))
        (corfu--restore-on-next-command)
        (with-selected-window (display-buffer buf t)
          (setq other-window-scroll-buffer (current-buffer))
          (save-restriction
            (widen)
            (if (bufferp (car loc))
                (goto-char (cdr loc))
              (goto-char (point-min))
              (forward-line (1- (cdr loc))))
            (set-window-start nil (point)))))
    (user-error "No candidate location available")))

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
    ;; XXX Is the :exit-function handling sufficient?
    (when-let (exit (plist-get corfu--extra-properties :exit-function))
      (funcall exit newstr 'finished))
    (completion-in-region-mode -1)))

(defun corfu--setup ()
  "Setup Corfu completion state."
  ;; Keep completion alive when popup is shown (disable predicate check!)
  (remove-hook 'post-command-hook #'completion-in-region--postch)
  (setq corfu--extra-properties completion-extra-properties)
  (setcdr (assq #'completion-in-region-mode minor-mode-overriding-map-alist) corfu-map)
  (add-hook 'pre-command-hook #'corfu--pre-command-hook nil 'local)
  (add-hook 'post-command-hook #'corfu--post-command-hook nil 'local))

(defun corfu--teardown ()
  "Teardown Corfu."
  (remove-hook 'pre-command-hook #'corfu--pre-command-hook 'local)
  (remove-hook 'post-command-hook #'corfu--post-command-hook 'local)
  (mapc #'delete-overlay corfu--overlays)
  (mapc #'kill-local-variable corfu--state-vars))

(defun corfu--mode-hook ()
  "Corfu mode hook."
  (if completion-in-region-mode
      (corfu--setup)
    (corfu--teardown)))

(defun corfu--completion-in-region (&rest args)
  "Corfu completion in region function passing ARGS to `completion--in-region'."
  (let ((completion-show-inline-help)
        (completion-auto-help))
    (apply #'completion--in-region args)))

;;;###autoload
(define-minor-mode corfu-mode
  "Completion Overlay Region FUnction"
  :local t
  (remove-hook 'completion-in-region-mode-hook #'corfu--mode-hook 'local)
  (kill-local-variable 'completion-in-region-function)
  (when corfu-mode
    (add-hook 'completion-in-region-mode-hook #'corfu--mode-hook nil 'local)
    (setq-local completion-in-region-function #'corfu--completion-in-region)))

(provide 'corfu)
;;; corfu.el ends here
