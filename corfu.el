;;; corfu.el --- Completion Overlay Region FUnction -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.13
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

;; Corfu enhances the default completion in region function with a
;; completion overlay. The current candidates are shown in a popup
;; below or above the point. Corfu can be considered the minimalistic
;; completion-in-region counterpart of Vertico.

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup corfu nil
  "Completion Overlay Region FUnction."
  :group 'convenience
  :prefix "corfu-")

(defcustom corfu-count 10
  "Maximal number of candidates to show."
  :type 'integer)

(defcustom corfu-min-width 15
  "Popup minimum width in characters."
  :type 'integer)

(defcustom corfu-max-width 100
  "Popup maximum width in characters."
  :type 'integer)

(defcustom corfu-cycle nil
  "Enable cycling for `corfu-next' and `corfu-previous'."
  :type 'boolean)

(defcustom corfu-continue-commands
  ;; nil is undefined command
  '(nil completion-at-point "\\`corfu-" "\\`scroll-other-window"
        universal-argument universal-argument-more digit-argument)
  "Continue Corfu completion after executing these commands."
  :type '(repeat (choice regexp symbol)))

(defcustom corfu-commit-predicate t
  "Automatically commit the selected candidate if the predicate returns t."
  :type '(choice boolean (function :tag "Predicate function")))

(defcustom corfu-quit-at-boundary nil
  "Automatically quit at completion field/word boundary.
If automatic quitting is disabled, Orderless filtering is facilitated since a
filter string with spaces is allowed."
  :type 'boolean)

(defcustom corfu-quit-no-match 1.0
  "Automatically quit if no matching candidate is found.
If a floating point number, quit on no match only if the auto-started
completion began less than that number of seconds ago."
  :type '(choice boolean float))

(defcustom corfu-excluded-modes nil
  "List of modes excluded by `corfu-global-mode'."
  :type '(repeat symbol))

(defcustom corfu-margin-width 0.6
  "Width of the margin in units of the character width."
  :type 'float)

(defcustom corfu-bar-width 0.2
  "Width of the bar in units of the character width."
  :type 'float)

(defcustom corfu-echo-documentation 0.25
  "Show documentation string in the echo area after that number of seconds."
  :type '(choice boolean float))

(defcustom corfu-auto-prefix 3
  "Minimum length of prefix for auto completion."
  :type 'integer)

(defcustom corfu-auto-delay 0.2
  "Delay for auto completion."
  :type 'float)

(defcustom corfu-auto-commands
  '("self-insert-command\\'")
  "Commands which initiate auto completion."
  :type '(repeat (choice regexp symbol)))

(defcustom corfu-auto nil
  "Enable auto completion."
  :type 'boolean)

(defgroup corfu-faces nil
  "Faces used by Corfu."
  :group 'corfu
  :group 'faces)

(defface corfu-background
  '((((class color) (min-colors 88) (background dark)) :background "#191a1b")
    (((class color) (min-colors 88) (background light)) :background "#f0f0f0")
    (t :background "gray"))
  "Face used for the popup background.")

(defface corfu-current
  '((((class color) (min-colors 88) (background dark))
     :background "#00415e" :foreground "white")
    (((class color) (min-colors 88) (background light))
     :background "#c0efff" :foreground "black")
    (t :background "blue" :foreground "white"))
  "Face used to highlight the currently selected candidate.")

(defface corfu-bar
  '((((class color) (min-colors 88) (background dark)) :background "#a8a8a8")
    (((class color) (min-colors 88) (background light)) :background "#505050")
    (t :background "gray"))
  "The background color is used for the scrollbar indicator.")

(defface corfu-border
  '((((class color) (min-colors 88) (background dark)) :background "#323232")
    (((class color) (min-colors 88) (background light)) :background "#d7d7d7")
    (t :background "gray"))
  "The background color used for the thin border.")

(defface corfu-echo
  '((t :inherit completions-annotations))
  "Face used to for echo area messages.")

(defvar corfu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap beginning-of-buffer] #'corfu-first)
    (define-key map [remap end-of-buffer] #'corfu-last)
    (define-key map [remap scroll-down-command] #'corfu-scroll-down)
    (define-key map [remap scroll-up-command] #'corfu-scroll-up)
    (define-key map [remap next-line] #'corfu-next)
    (define-key map [remap previous-line] #'corfu-previous)
    (define-key map [remap completion-at-point] #'corfu-complete)
    (define-key map [down] #'corfu-next)
    (define-key map [up] #'corfu-previous)
    ;; XXX [tab] is bound because of org-mode
    ;; The binding should be removed from org-mode-map.
    (define-key map [tab] #'corfu-complete)
    (define-key map "\en" #'corfu-next)
    (define-key map "\ep" #'corfu-previous)
    (define-key map "\e\e\e" #'corfu-quit)
    (define-key map "\C-g" #'corfu-quit)
    (define-key map "\r" #'corfu-insert)
    (define-key map "\t" #'corfu-complete)
    (define-key map "\eg" #'corfu-show-location)
    (define-key map "\eh" #'corfu-show-documentation)
    map)
  "Corfu keymap used when popup is shown.")

(defvar corfu--auto-timer nil
  "Auto completion timer.")

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

(defvar-local corfu--overlay nil
  "Current candidate overlay.")

(defvar-local corfu--extra nil
  "Extra completion properties.")

(defvar-local corfu--auto-start nil
  "Auto completion start time.")

(defvar-local corfu--echo-timer nil
  "Echo area message timer.")

(defvar corfu--frame nil
  "Popup frame.")

(defconst corfu--state-vars
  '(corfu--base
    corfu--candidates
    corfu--highlight
    corfu--index
    corfu--input
    corfu--total
    corfu--overlay
    corfu--extra
    corfu--auto-start
    corfu--echo-timer)
  "Buffer-local state variables used by Corfu.")

(defvar corfu--frame-parameters
  '((no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width . t)
    (min-height . t)
    (width . 0)
    (height . 0)
    (border-width . 0)
    (child-frame-border-width . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (no-other-frame . t)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . nil)
    (minibuffer . nil)
    (visibility . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t))
  "Default child frame parameters.")

(defvar corfu--buffer-parameters
  '((mode-line-format . nil)
    (header-line-format . nil)
    (tab-line-format . nil)
    (tab-bar-format . nil) ;; Emacs 28 tab-bar-format
    (frame-title-format . "")
    (truncate-lines . t)
    (cursor-in-non-selected-windows . nil)
    (cursor-type . nil)
    (show-trailing-whitespace . nil)
    (display-line-numbers . nil)
    (left-fringe-width . nil)
    (right-fringe-width . nil)
    (left-margin-width . 0)
    (right-margin-width . 0)
    (fringes-outside-margins . 0)
    (buffer-read-only . t))
  "Default child frame buffer parameters.")

(defvar corfu--mouse-ignore-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (define-key map (vector (intern (format "%s-%s" k (1+ i)))) #'ignore)))
    map)
  "Ignore all mouse clicks.")

(defun corfu--popup-redirect-focus ()
  "Redirect focus from popup."
  (redirect-frame-focus corfu--frame (frame-parent corfu--frame)))

(defun corfu--make-buffer (content)
  "Create corfu buffer with CONTENT."
  (let ((fr face-remapping-alist)
        (buffer (get-buffer-create " *corfu*")))
    (with-current-buffer buffer
      ;;; XXX HACK install redirect focus hook
      (add-hook 'pre-command-hook #'corfu--popup-redirect-focus nil 'local)
      ;;; XXX HACK install mouse ignore map
      (use-local-map corfu--mouse-ignore-map)
      (dolist (var corfu--buffer-parameters)
        (set (make-local-variable (car var)) (cdr var)))
      (setq-local face-remapping-alist fr)
      (let ((inhibit-modification-hooks t)
            (inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))))
    buffer))

;; Function adapted from posframe.el by tumashu
(defun corfu--make-frame (x y width height content)
  "Show child frame at X/Y with WIDTH/HEIGHT and CONTENT."
  (let* ((window-min-height 1)
         (window-min-width 1)
         (x-gtk-resize-child-frames
          (let ((case-fold-search t))
            (and
             ;; XXX HACK to fix resizing on gtk3/gnome taken from posframe.el
             ;; More information:
             ;; * https://github.com/minad/corfu/issues/17
             ;; * https://gitlab.gnome.org/GNOME/mutter/-/issues/840
             ;; * https://lists.gnu.org/archive/html/emacs-devel/2020-02/msg00001.html
             (string-match-p "gtk3" system-configuration-features)
             (string-match-p "gnome\\|cinnamon" (or (getenv "XDG_CURRENT_DESKTOP")
                                                    (getenv "DESKTOP_SESSION") ""))
             'resize-mode)))
         (after-make-frame-functions)
         (edge (window-inside-pixel-edges))
         (lh (default-line-height))
         (x (max 0 (min (+ (car edge) x
                           (- (alist-get 'child-frame-border-width corfu--frame-parameters)))
                        (- (frame-pixel-width) width))))
         (yb (+ (cadr edge) (window-tab-line-height) y lh))
         (y (if (> (+ yb height lh lh) (frame-pixel-height))
                (- yb height lh 1)
              yb))
         (buffer (corfu--make-buffer content)))
    (unless (and (frame-live-p corfu--frame)
                 (eq (frame-parent corfu--frame) (window-frame)))
      (when corfu--frame (delete-frame corfu--frame))
      (setq corfu--frame (make-frame
                          `((parent-frame . ,(window-frame))
                            (line-spacing . ,line-spacing)
                            ;; Set `internal-border-width' for Emacs 27
                            (internal-border-width
                             . ,(alist-get 'child-frame-border-width corfu--frame-parameters))
                            ,@corfu--frame-parameters))))
    ;; XXX HACK Setting the same frame-parameter/face-background is not a nop (BUG!).
    ;; Check explicitly before applying the setting.
    ;; Without the check, the frame flickers on Mac.
    ;; XXX HACK We have to apply the face background before adjusting the frame parameter,
    ;; otherwise the border is not updated (BUG!).
    (let* ((face (if (facep 'child-frame-border) 'child-frame-border 'internal-border))
	   (new (face-attribute 'corfu-border :background nil 'default)))
      (unless (equal (face-attribute face :background corfu--frame 'default) new)
	(set-face-background face new corfu--frame)))
    (let ((new (face-attribute 'corfu-background :background nil 'default)))
      (unless (equal (frame-parameter corfu--frame 'background-color) new)
	(set-frame-parameter corfu--frame 'background-color new)))
    (let ((win (frame-root-window corfu--frame)))
      (set-window-buffer win buffer)
      ;; Mark window as dedicated to prevent frame reuse (#60)
      (set-window-dedicated-p win t)
      ;; Disallow selection of root window (#63)
      (set-window-parameter win 'no-delete-other-windows t)
      (set-window-parameter win 'no-other-window t))
    ;; XXX HACK Make the frame invisible before moving the popup in order to avoid flicker.
    (unless (eq (cdr (frame-position corfu--frame)) y)
      (make-frame-invisible corfu--frame))
    (set-frame-position corfu--frame x y)
    (set-frame-size corfu--frame width height t)
    (make-frame-visible corfu--frame)))

(defun corfu--popup-show (pos lines &optional curr lo bar)
  "Show LINES as popup at POS, with CURR highlighted and scrollbar from LO to LO+BAR."
  (let* ((ch (default-line-height))
         (cw (round (* ch (frame-char-width)) (frame-char-height)))
         (mw (ceiling (* cw corfu-margin-width)))
         (bw (ceiling (* cw (min corfu-margin-width corfu-bar-width))))
         (margin (propertize " " 'display `(space :width (,mw))))
         (align (propertize " " 'display `(space :align-to (- right (,mw)))))
         (sbar (concat
                (propertize " " 'display `(space :width (,(- mw bw))))
                (propertize " " 'face 'corfu-bar 'display `(space :width (,bw)))))
         (width (min corfu-max-width
                     (frame-width)
                     (apply #'max corfu-min-width
                            (mapcar #'string-width lines))))
         (row 0)
         ;;; XXX HACK On Emacs 28 y-coordinate position computation is wrong if
         ;;; there exists a flymake underline overlay at that point. Therefore
         ;;; compute the y-coordinate at the line beginning.
         (x (or (car (posn-x-y (posn-at-point pos))) 0))
         (y (save-excursion
              (goto-char pos)
              (beginning-of-visual-line)
              (or (cdr (posn-x-y (posn-at-point))) 0))))
    (corfu--make-frame
     (- x mw) y
     (+ (* width cw) mw mw) (* (length lines) ch)
     (mapconcat (lambda (line)
                  (let ((str (concat
                              margin
                              (truncate-string-to-width line width)
                              align
                              (if (and lo (<= lo row (+ lo bar)))
                                  sbar margin))))
                    (when (eq row curr)
                      (add-face-text-property
                       0 (length str) 'corfu-current 'append str))
                    (setq row (1+ row))
                    str))
                lines "\n"))))

(defun corfu--popup-hide ()
  "Hide Corfu popup."
  (when (frame-live-p corfu--frame)
    (make-frame-invisible corfu--frame)
    (with-current-buffer (window-buffer (frame-root-window corfu--frame))
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun corfu--move-to-front (elem list)
  "Move ELEM to front of LIST."
  (if-let (found (member elem list))
      (let ((head (list (car found))))
        (nconc head (delq (setcar found nil) list)))
    list))

;; bug#47711: Deferred highlighting for `completion-all-completions'
;; XXX There is one complication: `completion--twq-all' already adds `completions-common-part'.
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
                (setq hl (lambda (x)
                           ;; `completion-pcm--hilit-commonality' sometimes throws an internal error
                           ;; for example when entering "/sudo:://u".
                           (condition-case nil
                               (completion-pcm--hilit-commonality pattern x)
                             (t x))))
                cands)))
    ;; Only advise orderless after it has been loaded to avoid load order issues
    (if (and (fboundp 'orderless-highlight-matches) (fboundp 'orderless-pattern-compiler))
        (cl-letf (((symbol-function 'orderless-highlight-matches)
                   (lambda (pattern cands)
                     (let ((regexps (orderless-pattern-compiler pattern)))
                       (setq hl (lambda (x) (orderless-highlight-matches regexps x))))
                     cands)))
          (cons (apply #'completion-all-completions args) hl))
      (cons (apply #'completion-all-completions args) hl))))

(defun corfu--sort-predicate (x y)
  "Sorting predicate which compares X and Y."
  (or (< (length x) (length y))
      (and (= (length x) (length y))
           (string< x y))))

(defmacro corfu--partition! (list form)
  "Evaluate FORM for every element and partition LIST."
  (let ((head1 (make-symbol "head1"))
        (head2 (make-symbol "head2"))
        (tail1 (make-symbol "tail1"))
        (tail2 (make-symbol "tail2")))
    `(let* ((,head1 (cons nil nil))
            (,head2 (cons nil nil))
            (,tail1 ,head1)
            (,tail2 ,head2))
       (while ,list
         (if (let ((it (car ,list))) ,form)
             (progn
               (setcdr ,tail1 ,list)
               (pop ,tail1))
           (setcdr ,tail2 ,list)
           (pop ,tail2))
         (pop ,list))
       (setcdr ,tail1 (cdr ,head2))
       (setcdr ,tail2 nil)
       (setq ,list (cdr ,head1)))))

(defun corfu--move-prefix-candidates-to-front (field candidates)
  "Move CANDIDATES which match prefix of FIELD to the beginning."
  (let* ((word (car (split-string field)))
         (len (length word)))
    (corfu--partition! candidates
                       (and (>= (length it) len)
                            (eq t (compare-strings word 0 len it 0 len))))))

(defun corfu--filter-files (files)
  "Filter FILES by `completion-ignored-extensions'."
  (let ((re (concat "\\(?:\\(?:\\`\\|/\\)\\.\\.?/\\|"
                    (regexp-opt completion-ignored-extensions)
                    "\\)\\'")))
    (or (seq-remove (lambda (x) (string-match-p re x)) files) files)))

(defun corfu--recompute-candidates (str metadata pt table pred)
  "Recompute candidates from STR, METADATA, PT, TABLE and PRED."
  ;; Redisplay such that the input becomes immediately visible before the
  ;; expensive candidate recomputation is performed (Issue #48). See also
  ;; corresponding vertico#89.
  (redisplay)
  (pcase-let* ((before (substring str 0 pt))
               (after (substring str pt))
               ;; bug#47678: `completion-boundaries` fails for `partial-completion`
               ;; if the cursor is moved between the slashes of "~//".
               ;; See also vertico.el which has the same issue.
               (bounds (or (condition-case nil
                               (completion-boundaries before
                                                      table
                                                      pred
                                                      after)
                             (t (cons 0 (length after))))))
               (field (substring str (car bounds) (+ pt (cdr bounds))))
               (completing-file (eq (corfu--metadata-get metadata 'category) 'file))
               (`(,all . ,hl) (corfu--all-completions str table pred pt metadata))
               (base (or (when-let (z (last all)) (prog1 (cdr z) (setcdr z nil))) 0)))
    ;; Filter the ignored file extensions. We cannot use modified predicate for this filtering,
    ;; since this breaks the special casing in the `completion-file-name-table' for `file-exists-p'
    ;; and `file-directory-p'.
    (when completing-file
      (setq all (corfu--filter-files all)))
    (setq all (if-let (sort (corfu--metadata-get metadata 'display-sort-function))
                  (funcall sort all)
                (sort all #'corfu--sort-predicate)))
    (unless (equal field "")
      (setq all (corfu--move-prefix-candidates-to-front field all))
      (when (and completing-file (not (string-suffix-p "/" field)))
        (setq all (corfu--move-to-front (concat field "/") all)))
      (setq all (corfu--move-to-front field all)))
    (list base (length all) all hl)))

(defun corfu--update-candidates (str metadata pt table pred)
  "Update candidates from STR, METADATA, PT, TABLE and PRED."
  (pcase (let ((while-no-input-ignore-events '(selection-request)))
           (while-no-input (corfu--recompute-candidates str metadata pt table pred)))
    ('nil (keyboard-quit))
    (`(,base ,total ,candidates ,hl)
     (setq corfu--input (cons str pt)
           corfu--candidates candidates
           corfu--base base
           corfu--total total
           corfu--highlight hl))))

(defun corfu--match-symbol-p (pattern sym)
  "Return non-nil if SYM is matching an element of the PATTERN list."
  (and (symbolp sym)
       (seq-some (lambda (x)
                   (if (symbolp x)
                       (eq sym x)
                     (string-match-p x (symbol-name sym))))
                 pattern)))

(defun corfu-quit ()
  "Quit Corfu completion."
  (interactive)
  (completion-in-region-mode -1))

(defun corfu--affixate (metadata candidates)
  "Annotate CANDIDATES with annotation function specified by METADATA."
  (if-let (aff (or (corfu--metadata-get metadata 'affixation-function)
                   (plist-get corfu--extra :affixation-function)))
      (funcall aff candidates)
    (if-let (ann (or (corfu--metadata-get metadata 'annotation-function)
                     (plist-get corfu--extra :annotation-function)))
        (mapcar (lambda (cand)
                  (let ((suffix (or (funcall ann cand) "")))
                    (list cand ""
                          ;; The default completion UI adds the `completions-annotations' face
                          ;; if no other faces are present.
                          (if (text-property-not-all 0 (length suffix) 'face nil suffix)
                              suffix
                            (propertize suffix 'face 'completions-annotations)))))
                candidates)
      candidates)))

;; XXX Do not use `completion-metadata-get' in order to avoid Marginalia.
;; The Marginalia annotators are way to heavy for the Corfu popup!
(defun corfu--metadata-get (metadata prop)
  "Return PROP from METADATA."
  (cdr (assq prop metadata)))

(defun corfu--format-candidate (cand)
  "Format annotated CAND string."
  (replace-regexp-in-string
   "[ \t]*\n[ \t]*" " "
   (if (consp cand)
       (concat (cadr cand) (car cand) (caddr cand))
     cand)))

(defun corfu--show-candidates (beg end str metadata)
  "Update display given BEG, END, STR and METADATA."
  (let* ((start (min (max 0 (- corfu--index (/ corfu-count 2)))
                     (max 0 (- corfu--total corfu-count))))
         (curr (- corfu--index start))
         (last (min (+ start corfu-count) corfu--total))
         (bar (ceiling (* corfu-count corfu-count) corfu--total))
         (lo (min (- corfu-count bar 1) (floor (* corfu-count start) corfu--total)))
         (cands (funcall corfu--highlight (seq-subseq corfu--candidates start last)))
         (ann-cands (mapcar #'corfu--format-candidate (corfu--affixate metadata cands))))
    ;; Nonlinearity at the end and the beginning
    (when (/= start 0)
      (setq lo (max 1 lo)))
    (when (/= last corfu--total)
      (setq lo (min (- corfu-count bar 2) lo)))
    (corfu--popup-show (+ beg corfu--base) ann-cands curr (and (> corfu--total corfu-count) lo) bar)
    (when (>= curr 0)
      (corfu--echo-documentation (nth corfu--index corfu--candidates))
      (corfu--show-overlay beg end str (nth curr cands)))))

(defun corfu--show-overlay (beg end str cand)
  "Show current CAND as overlay given BEG, END and STR."
  (setq corfu--overlay (make-overlay beg end nil t t))
  (overlay-put corfu--overlay 'priority 1000)
  (overlay-put corfu--overlay 'window (selected-window))
  (overlay-put corfu--overlay 'display (concat (substring str 0 corfu--base) cand)))

(defun corfu--echo (msg)
  "Show MSG in echo area."
  (let ((message-log-max nil))
    (message "%s" (if (text-property-not-all 0 (length msg) 'face nil msg)
                      msg
                    (propertize msg 'face 'corfu-echo)))))

(defun corfu--echo-documentation (cand)
  "Show documentation string for CAND in echo area."
  (when-let* ((fun (and corfu-echo-documentation (plist-get corfu--extra :company-docsig)))
              (doc (funcall fun cand)))
    (if (eq corfu-echo-documentation t)
        (corfu--echo doc)
      (setq corfu--echo-timer (run-with-idle-timer corfu-echo-documentation
                                                   nil #'corfu--echo doc)))))

(defun corfu--update (msg)
  "Refresh Corfu UI, possibly printing a message with MSG."
  (pcase-let* ((`(,beg ,end ,table ,pred) completion-in-region--data)
               (pt (- (point) beg))
               (str (buffer-substring-no-properties beg end))
               (metadata (completion-metadata (substring str 0 pt) table pred))
               (initializing (not corfu--input))
               (continue (or (/= beg end)
                             (corfu--match-symbol-p corfu-continue-commands
                                                    this-command))))
    (when corfu--overlay
      (delete-overlay corfu--overlay)
      (setq corfu--overlay nil))
    (when corfu--echo-timer
      (cancel-timer corfu--echo-timer)
      (setq corfu--echo-timer nil))
    (cond
     ;; XXX Guard against errors during candidate generation.
     ;; Turn off completion immediately if there are errors
     ;; For example dabbrev throws error "No dynamic expansion ... found".
     ;; TODO Report this as a bug? Are completion tables supposed to throw errors?
     ((condition-case err
          ;; Only recompute when input changed and when input is non-empty
          (when (and continue (not (equal corfu--input (cons str pt))))
            (corfu--update-candidates str metadata pt table pred)
            nil)
        (t (message "%s" (error-message-string err))
           nil)))
     ((and initializing (not corfu--candidates))      ;; 1) Initializing, no candidates
      (funcall msg "No match")                        ;; => Show error message
      nil)
     ((and corfu--candidates                          ;; 2) There exist candidates
           (not (equal corfu--candidates (list str))) ;; &  Not a sole exactly matching candidate
           continue)                                  ;; &  Input is non-empty or continue command
      (corfu--show-candidates beg end str metadata)   ;; => Show candidates popup
      t)
     ;; 3) When after `completion-at-point/corfu-complete', no further completion is possible and the
     ;; current string is a valid match, exit with status 'finished.
     ((and (memq this-command '(corfu-complete completion-at-point))
           (not (stringp (try-completion str table pred)))
           ;; XXX We should probably use `completion-try-completion' here instead
           ;; but it does not work as well when completing in `shell-mode'.
           ;; (not (consp (completion-try-completion str table pred pt metadata)))
           (test-completion str table pred))
      (corfu--done str 'finished)
      nil)
     ((not (or corfu--candidates                      ;; 4) There are no candidates
               ;; When `corfu-quit-no-match' is a number of seconds and the auto completion wasn't
               ;; initiated too long ago, quit directly without showing the "No match" popup.
               (if (and corfu--auto-start (numberp corfu-quit-no-match))
                   (< (- (float-time) corfu--auto-start) corfu-quit-no-match)
                 (eq t corfu-quit-no-match))))
      (corfu--popup-show beg '(#("No match" 0 8 (face italic)))) ;; => Show confirmation popup
      t))))

(defun corfu--pre-command ()
  "Insert selected candidate unless command is marked to continue completion."
  (add-hook 'window-configuration-change-hook #'corfu-quit)
  (unless (or (< corfu--index 0) (corfu--match-symbol-p corfu-continue-commands this-command))
    (if (if (functionp corfu-commit-predicate)
            (funcall corfu-commit-predicate)
          corfu-commit-predicate)
        (corfu--insert 'exact)
      (setq corfu--index -1))))

(defun corfu--post-command ()
  "Refresh Corfu after last command."
  (remove-hook 'window-configuration-change-hook #'corfu-quit)
  (or (pcase completion-in-region--data
        (`(,beg ,end ,_table ,_pred)
         (when (and (eq (marker-buffer beg) (current-buffer)) (<= beg (point) end))
           (corfu--update #'minibuffer-message))))
      (corfu-quit)))

(defun corfu--goto (index)
  "Go to candidate with INDEX."
  (setq corfu--index (max -1 (min index (1- corfu--total)))
        ;; Reset auto start in order to disable the `corfu-quit-no-match' timer
        corfu--auto-start nil))

(defun corfu-next (&optional n)
  "Go forward N candidates."
  (interactive "p")
  (let ((index (+ corfu--index (or n 1))))
    (corfu--goto (if corfu-cycle
                     (1- (mod (1+ index) (1+ corfu--total)))
                   index))))

(defun corfu-previous (&optional n)
  "Go backward N candidates."
  (interactive "p")
  (corfu-next (- (or n 1))))

(defun corfu-scroll-down (&optional n)
  "Go back by N pages."
  (interactive "p")
  (corfu--goto (max 0 (- corfu--index (* (or n 1) corfu-count)))))

(defun corfu-scroll-up (&optional n)
  "Go forward by N pages."
  (interactive "p")
  (corfu-scroll-down (- (or n 1))))

(defun corfu-first ()
  "Go to first candidate, or to the prompt when the first candidate is selected."
  (interactive)
  (corfu--goto (if (> corfu--index 0) 0 -1)))

(defun corfu-last ()
  "Go to last candidate."
  (interactive)
  (corfu--goto (1- corfu--total)))

(defun corfu--restore-on-next-command ()
  "Restore window configuration before next command."
  (let ((config (current-window-configuration))
        (other other-window-scroll-buffer)
        (restore (make-symbol "corfu--restore")))
    (fset restore
          (lambda ()
            (when (eq this-command #'corfu-quit)
              (setq this-command #'ignore))
            (remove-hook 'pre-command-hook restore)
            (setq other-window-scroll-buffer other)
            (set-window-configuration config)))
    (add-hook 'pre-command-hook restore)))

;; Company support, taken from `company.el', see `company-show-doc-buffer'.
(defun corfu-show-documentation ()
  "Show documentation of current candidate."
  (interactive)
  (when (< corfu--index 0)
    (user-error "No candidate selected"))
  (if-let* ((fun (plist-get corfu--extra :company-doc-buffer))
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
  (if-let* ((fun (plist-get corfu--extra :company-location))
            (loc (funcall fun (nth corfu--index corfu--candidates))))
      (let ((buf (or (and (bufferp (car loc)) (car loc)) (find-file-noselect (car loc) t))))
        (corfu--restore-on-next-command)
        (setq other-window-scroll-buffer buf)
        (with-selected-window (display-buffer buf t)
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
  (cond
   ;; Proceed with cycling
   (completion-cycling (completion-at-point))
   ;; Continue completion with selected candidate
   ((>= corfu--index 0) (corfu--insert nil))
   ;; Try to complete the current input string
   (t (pcase-let* ((`(,beg ,end ,table ,pred) completion-in-region--data)
                   (pt (max 0 (- (point) beg)))
                   (str (buffer-substring-no-properties beg end))
                   (metadata (completion-metadata (substring str 0 pt) table pred)))
        (pcase (completion-try-completion str table pred pt metadata)
          ;; Prefix completion made progress.
          ((and `(,newstr . ,newpt) (guard (not (equal str newstr))))
           (completion--replace beg end newstr)
           (goto-char (+ beg newpt)))
          ;; If we didn't make progress, the last command was `corfu-complete'
          ;; and we are not at completion boundary, continue with the first candidate.
          ((guard (and (eq last-command #'corfu-complete)
                       (> (length str) corfu--base)
                       (> corfu--total 0)))
           (completion--replace beg end
                                (concat (substring str 0 corfu--base)
                                        (substring-no-properties
                                         (car corfu--candidates))))))))))

(defun corfu--insert (status)
  "Insert current candidate, exit with STATUS if non-nil."
  (pcase-let* ((`(,beg ,end ,table ,pred) completion-in-region--data)
               (str (buffer-substring-no-properties beg end)))
    ;; Replace if candidate is selected or if current input is not valid completion.
    ;; For example str can be a valid path, e.g., ~/dir/.
    (when (or (>= corfu--index 0) (equal str "")
              (not (test-completion str table pred)))
      ;; XXX There is a small bug here, depending on interpretation.
      ;; When completing "~/emacs/master/li|/calc" where "|" is the
      ;; cursor, then the candidate only includes the prefix
      ;; "~/emacs/master/lisp/", but not the suffix "/calc". Default
      ;; completion has the same problem when selecting in the
      ;; *Completions* buffer. See bug#48356.
      (setq str (concat (substring str 0 corfu--base)
                        (substring-no-properties
                         (nth (max 0 corfu--index) corfu--candidates))))
      (completion--replace beg end str)
      (setq corfu--index -1)) ;; Reset selection, but continue completion.
    (when status (corfu--done str status)))) ;; Exit with status

(defun corfu--done (str status)
  "Call the `:exit-function' with STR and STATUS and exit completion."
  ;; XXX Is the :exit-function handling sufficient?
  (when-let (exit (plist-get corfu--extra :exit-function))
    (funcall exit str status))
  (corfu-quit))

(defun corfu-insert ()
  "Insert current candidate."
  (interactive)
  (if (> corfu--total 0)
      (corfu--insert 'finished)
    (corfu-quit)))

(defun corfu--setup ()
  "Setup Corfu completion state."
  (when completion-in-region-mode
    (setq corfu--extra completion-extra-properties)
    (setcdr (assq #'completion-in-region-mode minor-mode-overriding-map-alist) corfu-map)
    (add-hook 'pre-command-hook #'corfu--pre-command nil 'local)
    (add-hook 'post-command-hook #'corfu--post-command nil 'local)
    (let ((sym (make-symbol "corfu--teardown"))
          (buf (current-buffer)))
      (fset sym (lambda ()
                  ;; Ensure that the teardown runs in the correct buffer, if still alive.
                  (unless completion-in-region-mode
                    (remove-hook 'completion-in-region-mode-hook sym)
                    (with-current-buffer (if (buffer-live-p buf) buf (current-buffer))
                      (corfu--teardown)))))
      (add-hook 'completion-in-region-mode-hook sym))))

(defun corfu--teardown ()
  "Teardown Corfu."
  ;; Redisplay such that the input becomes immediately visible before the popup
  ;; hiding, which is slow (Issue #48). See also corresponding vertico#89.
  (redisplay)
  (corfu--popup-hide)
  (remove-hook 'window-configuration-change-hook #'corfu-quit)
  (remove-hook 'pre-command-hook #'corfu--pre-command 'local)
  (remove-hook 'post-command-hook #'corfu--post-command 'local)
  (when corfu--overlay (delete-overlay corfu--overlay))
  (when corfu--echo-timer (cancel-timer corfu--echo-timer))
  (mapc #'kill-local-variable corfu--state-vars))

(defun corfu--completion-in-region (&rest args)
  "Corfu completion in region function passing ARGS to `completion--in-region'."
  (if (not (display-graphic-p))
      ;; XXX Warning this can result in an endless loop when `completion-in-region-function'
      ;; is set *globally* to `corfu--completion-in-region'. This should never happen.
      (apply (default-value 'completion-in-region-function) args)
    ;; Restart the completion. This can happen for example if C-M-/
    ;; (`dabbrev-completion') is pressed while the Corfu popup is already open.
    (when (and completion-in-region-mode (not completion-cycling))
      (corfu-quit))
    (let ((completion-show-inline-help)
          (completion-auto-help)
          ;; XXX Disable original predicate check, keep completion alive when
          ;; popup is shown. Since the predicate is set always, it is ensured
          ;; that `completion-in-region-mode' is turned on.
          (completion-in-region-mode-predicate
           (or (and corfu-quit-at-boundary
                    completion-in-region-mode-predicate)
               (lambda () t))))
      (prog1 (apply #'completion--in-region args)
        (corfu--setup)))))

(defun corfu--auto-complete (buffer)
  "Initiate auto completion after delay in BUFFER."
  (setq corfu--auto-timer nil)
  (when (and (not completion-in-region-mode)
             (eq (current-buffer) buffer))
    (pcase (run-hook-wrapped 'completion-at-point-functions
                             #'completion--capf-wrapper 'all)
      ((and `(,fun ,beg ,end ,table . ,plist)
            (guard (integer-or-marker-p beg))
            (guard (<= beg (point) end))
            (guard (>= (- (point) beg) corfu-auto-prefix)))
       (let ((completion-extra-properties plist)
             (completion-in-region-mode-predicate
              (if corfu-quit-at-boundary
                 (lambda ()
                   (when-let (newbeg (car-safe (funcall fun)))
                     (= newbeg beg)))
                (lambda () t))))
         (setq completion-in-region--data `(,(copy-marker beg) ,(copy-marker end t)
                                            ,table ,(plist-get plist :predicate))
               corfu--auto-start (float-time))
         (completion-in-region-mode 1)
         (corfu--setup)
         (unless (corfu--update #'ignore)
           (corfu-quit)))))))

(defun corfu--auto-post-command ()
  "Post command hook which initiates auto completion."
  (when corfu--auto-timer
    (cancel-timer corfu--auto-timer)
    (setq corfu--auto-timer nil))
  (when (and (not completion-in-region-mode)
             (display-graphic-p)
             (corfu--match-symbol-p corfu-auto-commands this-command))
    (setq corfu--auto-timer (run-with-idle-timer corfu-auto-delay nil
                                                 #'corfu--auto-complete
                                                 (current-buffer)))))

;;;###autoload
(define-minor-mode corfu-mode
  "Completion Overlay Region FUnction"
  :global nil :group 'corfu
  (if corfu-mode
      (progn
        (and corfu-auto (add-hook 'post-command-hook #'corfu--auto-post-command nil 'local))
        (setq-local completion-in-region-function #'corfu--completion-in-region))
    (remove-hook 'post-command-hook #'corfu--auto-post-command 'local)
    (kill-local-variable 'completion-in-region-function)))

;;;###autoload
(define-globalized-minor-mode corfu-global-mode corfu-mode corfu--on :group 'corfu)

(defun corfu--on ()
  "Turn `corfu-mode' on."
  (unless (or noninteractive
              (eq (aref (buffer-name) 0) ?\s)
              (memq major-mode corfu-excluded-modes))
    (corfu-mode 1)))

;; Emacs 28: Do not show Corfu commands with M-X
(dolist (sym '(corfu-next corfu-previous corfu-first corfu-last corfu-quit
               corfu-complete corfu-insert corfu-scroll-up corfu-scroll-down
               corfu-show-location corfu-show-documentation))
  (put sym 'completion-predicate #'ignore))

(provide 'corfu)
;;; corfu.el ends here
