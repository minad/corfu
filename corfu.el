;;; corfu.el --- Completion Overlay Region FUnction -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.34
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(defcustom corfu-scroll-margin 2
  "Number of lines at the top and bottom when scrolling.
The value should lie between 0 and corfu-count/2."
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

(defcustom corfu-on-exact-match 'insert
  "Configure how a single exact match should be handled."
  :type '(choice (const insert) (const quit) (const nil)))

(defcustom corfu-continue-commands
  ;; nil is undefined command
  '(nil ignore universal-argument universal-argument-more digit-argument
        "\\`corfu-" "\\`scroll-other-window")
  "Continue Corfu completion after executing these commands."
  :type '(repeat (choice regexp symbol)))

(defcustom corfu-preview-current 'insert
  "Preview currently selected candidate.
If the variable has the value `insert', the candidate is automatically
inserted on further input."
  :type '(choice boolean (const insert)))

(defcustom corfu-preselect-first t
  "Preselect first candidate."
  :type 'boolean)

(defcustom corfu-separator ?\s
  "Component separator character.
The character used for separating components in the input. The presence
of this separator character will inhibit quitting at completion
boundaries, so that any further characters can be entered. To enter the
first separator character, call `corfu-insert-separator' (bound to M-SPC
by default). Useful for multi-component completion styles such as
Orderless."
  :type 'character)

(defcustom corfu-quit-at-boundary 'separator
  "Automatically quit at completion boundary.
nil: Never quit at completion boundary.
t: Always quit at completion boundary.
separator: Quit at boundary if no `corfu-separator' has been inserted."
  :type '(choice boolean (const separator)))

(defcustom corfu-quit-no-match 'separator
  "Automatically quit if no matching candidate is found.
When staying alive even if there is no match a warning message is
shown in the popup.
nil: Stay alive even if there is no match.
t: Quit if there is no match.
separator: Only stay alive if there is no match and
`corfu-separator' has been inserted."
  :type '(choice boolean (const separator)))

(defcustom corfu-excluded-modes nil
  "List of modes excluded by `global-corfu-mode'."
  :type '(repeat symbol))

(defcustom corfu-left-margin-width 0.5
  "Width of the left margin in units of the character width."
  :type 'float)

(defcustom corfu-right-margin-width 0.5
  "Width of the right margin in units of the character width."
  :type 'float)

(defcustom corfu-bar-width 0.2
  "Width of the bar in units of the character width."
  :type 'float)

(defcustom corfu-margin-formatters nil
  "Registry for margin formatter functions.
Each function of the list is called with the completion metadata as
argument until an appropriate formatter is found. The function should
return a formatter function, which takes the candidate string and must
return a string, possibly an icon."
  :type 'hook)

(defcustom corfu-sort-function #'corfu-sort-length-alpha
  "Default sorting function, used if no `display-sort-function' is specified."
  :type `(choice
          (const :tag "No sorting" nil)
          (const :tag "By length and alpha" ,#'corfu-sort-length-alpha)
          (function :tag "Custom function")))

(defcustom corfu-sort-override-function nil
  "Override sort function which overrides the `display-sort-function'."
  :type '(choice (const nil) function))

(defcustom corfu-auto-prefix 3
  "Minimum length of prefix for auto completion.
The completion backend can override this with
:company-prefix-length."
  :type 'integer)

(defcustom corfu-auto-delay 0.2
  "Delay for auto completion."
  :type 'float)

(defcustom corfu-auto-commands
  '("self-insert-command\\'"
    c-electric-colon c-electric-lt-gt c-electric-slash c-scope-operator)
  "Commands which initiate auto completion."
  :type '(repeat (choice regexp symbol)))

(defcustom corfu-auto nil
  "Enable auto completion."
  :type 'boolean)

(defgroup corfu-faces nil
  "Faces used by Corfu."
  :group 'corfu
  :group 'faces)

(defface corfu-default
  '((((class color) (min-colors 88) (background dark)) :background "#191a1b")
    (((class color) (min-colors 88) (background light)) :background "#f0f0f0")
    (t :background "gray"))
  "Default face, foreground and background colors used for the popup.")

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

(defface corfu-annotations
  '((t :inherit completions-annotations))
  "Face used for annotations.")

(defface corfu-deprecated
  '((t :inherit shadow :strike-through t))
  "Face used for deprecated candidates.")

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
    (define-key map [remap keyboard-escape-quit] #'corfu-reset)
    ;; XXX [tab] is bound because of org-mode
    ;; The binding should be removed from org-mode-map.
    (define-key map [tab] #'corfu-complete)
    (define-key map "\M-n" #'corfu-next)
    (define-key map "\M-p" #'corfu-previous)
    (define-key map "\C-g" #'corfu-quit)
    (define-key map "\r" #'corfu-insert)
    (define-key map "\t" #'corfu-complete)
    (define-key map "\M-g" 'corfu-info-location)
    (define-key map "\M-h" 'corfu-info-documentation)
    (define-key map "\M- " #'corfu-insert-separator)
    map)
  "Corfu keymap used when popup is shown.")

(defvar corfu--auto-timer nil
  "Auto completion timer.")

(defvar-local corfu--candidates nil
  "List of candidates.")

(defvar-local corfu--metadata nil
  "Completion metadata.")

(defvar-local corfu--base ""
  "Base string, which is concatenated with the candidate.")

(defvar-local corfu--total 0
  "Length of the candidate list `corfu--candidates'.")

(defvar-local corfu--highlight #'identity
  "Deferred candidate highlighting function.")

(defvar-local corfu--index -1
  "Index of current candidate or negative for prompt selection.")

(defvar-local corfu--preselect -1
  "Index of preselected candidate, negative for prompt selection.")

(defvar-local corfu--scroll 0
  "Scroll position.")

(defvar-local corfu--input nil
  "Cons of last prompt contents and point.")

(defvar-local corfu--preview-ov nil
  "Current candidate overlay.")

(defvar-local corfu--extra nil
  "Extra completion properties.")

(defvar-local corfu--change-group nil
  "Undo change group.")

(defvar corfu--frame nil
  "Popup frame.")

(defconst corfu--state-vars
  '(corfu--base
    corfu--candidates
    corfu--highlight
    corfu--index
    corfu--preselect
    corfu--scroll
    corfu--input
    corfu--total
    corfu--preview-ov
    corfu--extra
    corfu--change-group
    corfu--metadata)
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
    (fringe-indicator-alist . nil)
    (indicate-empty-lines . nil)
    (indicate-buffer-boundaries . nil)
    (buffer-read-only . t))
  "Default child frame buffer parameters.")

(defvar corfu--mouse-ignore-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (define-key map (vector (intern (format "%s-%s" k (1+ i)))) #'ignore)))
    map)
  "Ignore all mouse clicks.")

(defun corfu--make-buffer (name)
  "Create buffer with NAME."
  (let ((fr face-remapping-alist)
        (ls line-spacing)
        (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      ;;; XXX HACK install mouse ignore map
      (use-local-map corfu--mouse-ignore-map)
      (dolist (var corfu--buffer-parameters)
        (set (make-local-variable (car var)) (cdr var)))
      (setq-local face-remapping-alist (copy-tree fr)
                  line-spacing ls)
      (cl-pushnew 'corfu-default (alist-get 'default face-remapping-alist))
      buffer)))

;; Function adapted from posframe.el by tumashu
(defvar x-gtk-resize-child-frames) ;; Not present on non-gtk builds
(defun corfu--make-frame (frame x y width height buffer)
  "Show BUFFER in child frame at X/Y with WIDTH/HEIGHT.
FRAME is the existing frame."
  (when-let (timer (and (frame-live-p frame)
                        (frame-parameter frame 'corfu--hide-timer)))
    (cancel-timer timer)
    (set-frame-parameter frame 'corfu--hide-timer nil))
  (let* ((window-min-height 1)
         (window-min-width 1)
         (inhibit-redisplay t)
         (x-gtk-resize-child-frames
          (let ((case-fold-search t))
            (and
             ;; XXX HACK to fix resizing on gtk3/gnome taken from posframe.el
             ;; More information:
             ;; * https://github.com/minad/corfu/issues/17
             ;; * https://gitlab.gnome.org/GNOME/mutter/-/issues/840
             ;; * https://lists.gnu.org/archive/html/emacs-devel/2020-02/msg00001.html
             (string-match-p "gtk3" system-configuration-features)
             (string-match-p "gnome\\|cinnamon"
                             (or (getenv "XDG_CURRENT_DESKTOP")
                                 (getenv "DESKTOP_SESSION") ""))
             'resize-mode)))
         (after-make-frame-functions)
         (parent (window-frame)))
    (unless (and (frame-live-p frame) (eq (frame-parent frame) parent))
      (when frame (delete-frame frame))
      (setq frame (make-frame
                   `((parent-frame . ,parent)
                     (minibuffer . ,(minibuffer-window parent))
                     ;; Set `internal-border-width' for Emacs 27
                     (internal-border-width
                      . ,(alist-get 'child-frame-border-width corfu--frame-parameters))
                     ,@corfu--frame-parameters))))
    ;; XXX HACK Setting the same frame-parameter/face-background is not a nop.
    ;; Check before applying the setting. Without the check, the frame flickers
    ;; on Mac. We have to apply the face background before adjusting the frame
    ;; parameter, otherwise the border is not updated (BUG?).
    (let* ((face (if (facep 'child-frame-border) 'child-frame-border 'internal-border))
           (new (face-attribute 'corfu-border :background nil 'default)))
      (unless (equal (face-attribute face :background frame 'default) new)
        (set-face-background face new frame)))
    (let ((new (face-attribute 'corfu-default :background nil 'default)))
      (unless (equal (frame-parameter frame 'background-color) new)
        (set-frame-parameter frame 'background-color new)))
    (let ((win (frame-root-window frame)))
      (set-window-buffer win buffer)
      ;; Disallow selection of root window (#63)
      (set-window-parameter win 'no-delete-other-windows t)
      (set-window-parameter win 'no-other-window t)
      ;; Mark window as dedicated to prevent frame reuse (#60)
      (set-window-dedicated-p win t))
    ;; XXX HACK: Child frame popup behavior improved on Emacs 29.
    ;; It seems we may not need the Emacs 27/28 hacks anymore.
    (if (eval-when-compile (< emacs-major-version 29))
        (let (inhibit-redisplay)
          (set-frame-size frame width height t)
          (if (frame-visible-p frame)
              ;; XXX HACK Avoid flicker when frame is already visible.
              ;; Redisplay, wait for resize and then move the frame.
              (unless (equal (frame-position frame) (cons x y))
                (redisplay 'force)
                (sleep-for 0.01)
                (set-frame-position frame x y))
            ;; XXX HACK: Force redisplay, otherwise the popup sometimes does not
            ;; display content.
            (set-frame-position frame x y)
            (redisplay 'force)
            (make-frame-visible frame)))
      (set-frame-size frame width height t)
      (unless (equal (frame-position frame) (cons x y))
        (set-frame-position frame x y))
      (unless (frame-visible-p frame)
        (make-frame-visible frame)))
    (redirect-frame-focus frame parent)
    frame))

(defun corfu--popup-show (pos off width lines &optional curr lo bar)
  "Show LINES as popup at POS - OFF.
WIDTH is the width of the popup.
The current candidate CURR is highlighted.
A scroll bar is displayed from LO to LO+BAR."
  (let ((lh (default-line-height)))
    (with-current-buffer (corfu--make-buffer " *corfu*")
      (let* ((ch (default-line-height))
             (cw (default-font-width))
             (ml (ceiling (* cw corfu-left-margin-width)))
             (mr (ceiling (* cw corfu-right-margin-width)))
             (bw (ceiling (min mr (* cw corfu-bar-width))))
             (marginl (and (> ml 0) (propertize " " 'display `(space :width (,ml)))))
             (marginr (and (> mr 0) (propertize " " 'display `(space :align-to right))))
             (sbar (when (> bw 0)
                     (concat (propertize " " 'display `(space :align-to (- right (,mr))))
                             (propertize " " 'display `(space :width (,(- mr bw))))
                             (propertize " " 'face 'corfu-bar 'display `(space :width (,bw))))))
             (pos (posn-x-y (posn-at-point pos)))
             (width (+ (* width cw) ml mr))
             ;; XXX HACK: Minimum popup height must be at least 1 line of the
             ;; parent frame (#261).
             (height (max lh (* (length lines) ch)))
             (edge (window-inside-pixel-edges))
             (border (alist-get 'child-frame-border-width corfu--frame-parameters))
             (x (max 0 (min (+ (car edge) (- (or (car pos) 0) ml (* cw off) border))
                            (- (frame-pixel-width) width))))
             (yb (+ (cadr edge) (window-tab-line-height) (or (cdr pos) 0) lh))
             (y (if (> (+ yb (* corfu-count ch) lh lh) (frame-pixel-height))
                    (- yb height lh border border)
                  yb))
             (row 0))
        (with-silent-modifications
          (erase-buffer)
          (insert (mapconcat (lambda (line)
                               (let ((str (concat marginl line
                                                  (if (and lo (<= lo row (+ lo bar)))
                                                      sbar
                                                    marginr))))
                                 (when (eq row curr)
                                   (add-face-text-property
                                    0 (length str) 'corfu-current 'append str))
                                 (cl-incf row)
                                 str))
                             lines "\n"))
          (goto-char (point-min)))
        (setq corfu--frame (corfu--make-frame corfu--frame x y
                                              width height (current-buffer)))))))

(defun corfu--hide-frame-deferred (frame)
  "Deferred hiding of child FRAME."
  (when (and (frame-live-p frame) (frame-visible-p frame))
    (set-frame-parameter frame 'corfu--hide-timer nil)
    (make-frame-invisible frame)
    (with-current-buffer (window-buffer (frame-root-window frame))
      (with-silent-modifications
        (erase-buffer)))))

(defun corfu--hide-frame (frame)
  "Hide child FRAME."
  (when (and (frame-live-p frame) (frame-visible-p frame)
             (not (frame-parameter frame 'corfu--hide-timer)))
    (set-frame-parameter frame 'corfu--hide-timer
                         (run-at-time 0 nil #'corfu--hide-frame-deferred frame))))

(defun corfu--popup-hide ()
  "Hide Corfu popup."
  (corfu--hide-frame corfu--frame))

(defun corfu--popup-support-p ()
  "Return non-nil if child frames are supported."
  (display-graphic-p))

(defun corfu--move-to-front (elem list)
  "Move ELEM to front of LIST."
  (if-let (found (member elem list))
      (let ((head (list (car found))))
        (nconc head (delq (setcar found nil) list)))
    list))

;; bug#47711: Deferred highlighting for `completion-all-completions'
;; XXX There is one complication: `completion--twq-all' already adds
;; `completions-common-part'.
(defun corfu--all-completions (&rest args)
  "Compute all completions for ARGS with deferred highlighting."
  (cl-letf* ((orig-pcm (symbol-function #'completion-pcm--hilit-commonality))
             (orig-flex (symbol-function #'completion-flex-all-completions))
             ((symbol-function #'completion-flex-all-completions)
              (lambda (&rest args)
                ;; Unfortunately for flex we have to undo the deferred
                ;; highlighting, since flex uses the completion-score for
                ;; sorting, which is applied during highlighting.
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
                           ;; `completion-pcm--hilit-commonality' sometimes
                           ;; throws an internal error for example when entering
                           ;; "/sudo:://u".
                           (condition-case nil
                               (completion-pcm--hilit-commonality pattern x)
                             (t x))))
                cands)))
    ;; Only advise orderless after it has been loaded to avoid load order issues
    (if (and (fboundp 'orderless-highlight-matches)
             (fboundp 'orderless-pattern-compiler))
        (cl-letf (((symbol-function 'orderless-highlight-matches)
                   (lambda (pattern cands)
                     (let ((regexps (orderless-pattern-compiler pattern)))
                       (setq hl (lambda (x) (orderless-highlight-matches regexps x))))
                     cands)))
          (cons (apply #'completion-all-completions args) hl))
      (cons (apply #'completion-all-completions args) hl))))

(defsubst corfu--length-string< (x y)
  "Sorting predicate which compares X and Y first by length then by `string<'."
  (or (< (length x) (length y)) (and (= (length x) (length y)) (string< x y))))

(defun corfu-sort-length-alpha (list)
  "Sort LIST by length and alphabetically."
  (sort list #'corfu--length-string<))

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
  (let* ((word (substring field 0
                          (seq-position field corfu-separator)))
         (len (length word)))
    (corfu--partition!
     candidates
     (and (>= (length it) len)
          (eq t (compare-strings word 0 len it 0 len
                                 completion-ignore-case))))))

(defun corfu--sort-function ()
  "Return the sorting function."
  (or corfu-sort-override-function
      (corfu--metadata-get 'display-sort-function)
      corfu-sort-function))

(defun corfu--recompute (str pt table pred)
  "Recompute state from STR, PT, TABLE and PRED."
  (pcase-let* ((before (substring str 0 pt))
               (after (substring str pt))
               (corfu--metadata (completion-metadata before table pred))
               ;; bug#47678: `completion-boundaries` fails for `partial-completion`
               ;; if the cursor is moved between the slashes of "~//".
               ;; See also vertico.el which has the same issue.
               (bounds (or (condition-case nil
                               (completion-boundaries before table pred after)
                             (t (cons 0 (length after))))))
               (field (substring str (car bounds) (+ pt (cdr bounds))))
               (completing-file (eq (corfu--metadata-get 'category) 'file))
               (`(,all . ,hl) (corfu--all-completions str table pred pt corfu--metadata))
               (base (or (when-let (z (last all)) (prog1 (cdr z) (setcdr z nil))) 0))
               (corfu--base (substring str 0 base)))
    ;; Filter the ignored file extensions. We cannot use modified predicate for
    ;; this filtering, since this breaks the special casing in the
    ;; `completion-file-name-table' for `file-exists-p' and `file-directory-p'.
    (when completing-file (setq all (completion-pcm--filename-try-filter all)))
    (setq all (delete-consecutive-dups (funcall (or (corfu--sort-function) #'identity) all)))
    (setq all (corfu--move-prefix-candidates-to-front field all))
    (when (and completing-file (not (string-suffix-p "/" field)))
      (setq all (corfu--move-to-front (concat field "/") all)))
    (setq all (corfu--move-to-front field all))
    `((corfu--base . ,corfu--base)
      (corfu--metadata . ,corfu--metadata)
      (corfu--candidates . ,all)
      (corfu--total . ,(length all))
      (corfu--highlight . ,hl)
      ;; Select the prompt when the input is a valid completion
      ;; and if it is not equal to the first candidate.
      (corfu--preselect . ,(if (or (not corfu-preselect-first) (not all)
                                   (and (not (equal field (car all)))
                                        (not (and completing-file (equal (concat field "/") (car all))))
                                        (test-completion str table pred)))
                               -1 0)))))

(defun corfu--update (&optional interruptible)
  "Update state, optionally INTERRUPTIBLE."
  (pcase-let* ((`(,beg ,end ,table ,pred) completion-in-region--data)
               (pt (- (point) beg))
               (str (buffer-substring-no-properties beg end))
               (input (cons str pt)))
    (unless (equal corfu--input input)
      ;; Redisplay such that the input becomes immediately visible before the
      ;; expensive candidate recomputation is performed (Issue #48). See also
      ;; corresponding vertico#89.
      (when interruptible (redisplay))
      ;; Bind non-essential=t to prevent Tramp from opening new connections,
      ;; without the user explicitly requesting it via M-TAB.
      (pcase (let ((non-essential t))
               ;; XXX Guard against errors during candidate generation.
               ;; For example dabbrev throws error "No dynamic expansion ... found".
               ;; TODO Report this as a bug? Are completion tables supposed to throw errors?
               (condition-case err
                   (if interruptible
                       (while-no-input (corfu--recompute str pt table pred))
                     (corfu--recompute str pt table pred))
                 (error
                  (message "Corfu completion error: %s" (error-message-string err))
                  t)))
        ('nil (keyboard-quit))
        ((and state (pred consp))
         (dolist (s state) (set (car s) (cdr s)))
         (setq corfu--input input
               corfu--index corfu--preselect))))
    input))

(defun corfu--match-symbol-p (pattern sym)
  "Return non-nil if SYM is matching an element of the PATTERN list."
  (and (symbolp sym)
       (cl-loop for x in pattern
                thereis (if (symbolp x)
                            (eq sym x)
                          (string-match-p x (symbol-name sym))))))

(defun corfu-quit ()
  "Quit Corfu completion."
  (interactive)
  (completion-in-region-mode -1))

(defun corfu-reset ()
  "Reset Corfu completion.
This command can be executed multiple times by hammering the ESC key. If a
candidate is selected, unselect the candidate. Otherwise reset the input. If
there hasn't been any input, then quit."
  (interactive)
  (if (/= corfu--index corfu--preselect)
      (progn
        (corfu--goto -1)
        (setq this-command #'corfu-first))
    ;; Cancel all changes and start new change group.
    (cancel-change-group corfu--change-group)
    (activate-change-group (setq corfu--change-group (prepare-change-group)))
    (when (eq last-command #'corfu-reset) (corfu-quit))))

(defun corfu--affixate (cands)
  "Annotate CANDS with annotation function."
  (setq cands
        (if-let (aff (or (corfu--metadata-get 'affixation-function)
                         (plist-get corfu--extra :affixation-function)))
            (funcall aff cands)
          (if-let (ann (or (corfu--metadata-get 'annotation-function)
                           (plist-get corfu--extra :annotation-function)))
              (cl-loop for cand in cands collect
                       (let ((suffix (or (funcall ann cand) "")))
                         ;; The default completion UI adds the
                         ;; `completions-annotations' face if no other faces are
                         ;; present. We use a custom `corfu-annotations' face to
                         ;; allow further styling which fits better for popups.
                         (unless (text-property-not-all 0 (length suffix) 'face nil suffix)
                           (setq suffix (propertize suffix 'face 'corfu-annotations)))
                         (list cand "" suffix)))
            (cl-loop for cand in cands collect (list cand "" "")))))
  (let* ((dep (plist-get corfu--extra :company-deprecated))
         (completion-extra-properties corfu--extra)
         (mf (run-hook-with-args-until-success 'corfu-margin-formatters corfu--metadata)))
    (cl-loop for x in cands for (c . _) = x do
             (when mf
               (setf (cadr x) (funcall mf c)))
             (when (and dep (funcall dep c))
               (setcar x (setq c (substring c)))
               (add-face-text-property 0 (length c) 'corfu-deprecated 'append c)))
    (cons mf cands)))

(defun corfu--metadata-get (prop)
  "Return PROP from completion metadata."
  ;; Note: Do not use `completion-metadata-get' in order to avoid Marginalia.
  ;; The Marginalia annotators are too heavy for the Corfu popup!
  (cdr (assq prop corfu--metadata)))

(defun corfu--format-candidates (cands)
  "Format annotated CANDS."
  (setq cands
        (cl-loop for c in cands collect
                 (cl-loop for s in c collect
                          (replace-regexp-in-string "[ \t]*\n[ \t]*" " " s))))
  (let* ((cw (cl-loop for x in cands maximize (string-width (car x))))
         (pw (cl-loop for x in cands maximize (string-width (cadr x))))
         (sw (cl-loop for x in cands maximize (string-width (caddr x))))
         (width (+ pw cw sw))
         ;; -4 because of margins and some additional safety
         (max-width (min corfu-max-width (- (frame-width) 4))))
    (when (> width max-width)
      (setq sw (max 0 (- max-width pw cw))
            width (+ pw cw sw)))
    (when (< width corfu-min-width)
      (setq cw (+ cw (- corfu-min-width width))
            width corfu-min-width))
    (setq width (min width max-width))
    (list pw width
          (cl-loop for (cand prefix suffix) in cands collect
                   (truncate-string-to-width
                    (concat prefix
                            (make-string (max 0 (- pw (string-width prefix))) ?\s)
                            cand
                            (when (/= sw 0)
                              (make-string
                               (+ (max 0 (- cw (string-width cand)))
                                  (max 0 (- sw (string-width suffix))))
                               ?\s))
                            suffix)
                    width)))))

(defun corfu--compute-scroll ()
  "Compute new scroll position."
  (let ((off (max (min corfu-scroll-margin (/ corfu-count 2)) 0))
        (corr (if (= corfu-scroll-margin (/ corfu-count 2)) (1- (mod corfu-count 2)) 0)))
    (setq corfu--scroll (min (max 0 (- corfu--total corfu-count))
                             (max 0 (+ corfu--index off 1 (- corfu-count))
                                  (min (- corfu--index off corr) corfu--scroll))))))

(defun corfu--candidates-popup (pos)
  "Show candidates popup at POS."
  (corfu--compute-scroll)
  (pcase-let* ((last (min (+ corfu--scroll corfu-count) corfu--total))
               (bar (ceiling (* corfu-count corfu-count) corfu--total))
               (lo (min (- corfu-count bar 1) (floor (* corfu-count corfu--scroll) corfu--total)))
               (`(,mf . ,acands) (corfu--affixate (funcall corfu--highlight
                                   (seq-subseq corfu--candidates corfu--scroll last))))
               (`(,pw ,width ,fcands) (corfu--format-candidates acands))
               ;; Disable the left margin if a margin formatter is active.
               (corfu-left-margin-width (if mf 0 corfu-left-margin-width)))
    ;; Nonlinearity at the end and the beginning
    (when (/= corfu--scroll 0)
      (setq lo (max 1 lo)))
    (when (/= last corfu--total)
      (setq lo (min (- corfu-count bar 2) lo)))
    (corfu--popup-show (+ pos (length corfu--base)) pw width fcands (- corfu--index corfu--scroll)
                       (and (> corfu--total corfu-count) lo) bar)))

(defun corfu--preview-current (beg end)
  "Show current candidate as overlay given BEG and END."
  (when-let (cand (and corfu-preview-current (>= corfu--index 0)
                       (/= corfu--index corfu--preselect)
                       (nth corfu--index corfu--candidates)))
    (setq beg (+ beg (length corfu--base))
          corfu--preview-ov (make-overlay beg end nil))
    (overlay-put corfu--preview-ov 'priority 1000)
    (overlay-put corfu--preview-ov 'window (selected-window))
    (overlay-put corfu--preview-ov (if (= beg end) 'after-string 'display) cand)))

(defun corfu--exhibit (&optional auto)
  "Exhibit Corfu UI.
AUTO is non-nil when initializing auto completion."
  (pcase-let ((`(,beg ,end ,table ,pred) completion-in-region--data)
              (`(,str . ,pt) (corfu--update 'interruptible)))
    (cond
     ;; 1) Single exactly matching candidate and no further completion is possible.
     ((and (not (equal str ""))
           (equal (car corfu--candidates) str) (not (cdr corfu--candidates))
           (not (consp (completion-try-completion str table pred pt corfu--metadata)))
           (or auto corfu-on-exact-match))
      ;; Quit directly when initializing auto completion.
      (if (or auto (eq corfu-on-exact-match 'quit))
          (corfu-quit)
        (corfu--done str 'finished)))
     ;; 2) There exist candidates => Show candidates popup.
     (corfu--candidates
      (corfu--candidates-popup beg)
      (corfu--preview-current beg end)
      (redisplay 'force)) ;; XXX HACK Ensure that popup is redisplayed
     ;; 3) No candidates & corfu-quit-no-match & initialized => Confirmation popup.
     ((pcase-exhaustive corfu-quit-no-match
        ('t nil)
        ('nil corfu--input)
        ('separator (seq-contains-p (car corfu--input) corfu-separator)))
      (corfu--popup-show beg 0 8 '(#("No match" 0 8 (face italic))))
      (redisplay 'force)) ;; XXX HACK Ensure that popup is redisplayed
     ;; 4) No candidates & auto completing or initialized => Quit.
     ((or auto corfu--input) (corfu-quit)))))

(defun corfu--pre-command ()
  "Insert selected candidate unless command is marked to continue completion."
  (when corfu--preview-ov
    (delete-overlay corfu--preview-ov)
    (setq corfu--preview-ov nil))
  ;; Ensure that state is initialized before next Corfu command
  (when (and (symbolp this-command) (string-prefix-p "corfu-" (symbol-name this-command)))
    (corfu--update))
  (when (and (eq corfu-preview-current 'insert)
             (/= corfu--index corfu--preselect)
             ;; See the comment about `overriding-local-map' in `corfu--post-command'.
             (not (or overriding-terminal-local-map
                      (corfu--match-symbol-p corfu-continue-commands this-command))))
    (corfu--insert 'exact)))

(defun corfu-insert-separator ()
  "Insert a separator character, inhibiting quit on completion boundary.
See `corfu-separator' for more details."
  (interactive)
  (insert corfu-separator))

(defun corfu--continue-p ()
  "Continue completion?"
  (pcase-let ((pt (point))
              (`(,beg ,end . ,_) completion-in-region--data))
    (and beg end
         (eq (marker-buffer beg) (current-buffer))
         ;; Check ranges
         (<= beg pt end)
         (save-excursion
           (goto-char beg)
           (let ((inhibit-field-text-motion t))
             (<= (line-beginning-position) pt (line-end-position))))
         (or
          ;; We keep Corfu alive if a `overriding-terminal-local-map' is
          ;; installed, e.g., the `universal-argument-map'. It would be good to
          ;; think about a better criterion instead. Unfortunately relying on
          ;; `this-command' alone is insufficient, since the value of
          ;; `this-command' gets clobbered in the case of transient keymaps.
          overriding-terminal-local-map
          ;; Check if it is an explicitly listed continue command
          (corfu--match-symbol-p corfu-continue-commands this-command)
          (and (or (not corfu--input) (< beg end)) ;; Check for empty input
               (or (not corfu-quit-at-boundary) ;; Check separator or predicate
                   (and (eq corfu-quit-at-boundary 'separator)
                        (or (eq this-command #'corfu-insert-separator)
                            ;; with separator, any further chars allowed
                            (seq-contains-p (car corfu--input) corfu-separator)))
                   (funcall completion-in-region-mode--predicate)))))))

(defun corfu--post-command ()
  "Refresh Corfu after last command."
  (if (corfu--continue-p)
      (corfu--exhibit)
    (corfu-quit))
  (when corfu-auto
    (corfu--auto-post-command)))

(defun corfu--goto (index)
  "Go to candidate with INDEX."
  (setq corfu--index (max corfu--preselect (min index (1- corfu--total)))))

(defun corfu-next (&optional n)
  "Go forward N candidates."
  (interactive "p")
  (let ((index (+ corfu--index (or n 1))))
    (corfu--goto
     (cond
      ((not corfu-cycle) index)
      ((= corfu--total 0) -1)
      ((< corfu--preselect 0) (1- (mod (1+ index) (1+ corfu--total))))
      (t (mod index corfu--total))))))

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

(defun corfu-complete ()
  "Try to complete current input.
If a candidate is selected, insert it."
  (interactive)
  (pcase-let ((`(,beg ,end ,table ,pred) completion-in-region--data))
    (if (>= corfu--index 0)
        ;; Continue completion with selected candidate
        (progn
          (corfu--insert nil)
          ;; Exit with status 'finished if input is a valid match and no further
          ;; completion is possible. Furthermore treat the completion as
          ;; finished if we are at the end of a boundary, even if other longer
          ;; candidates would still match, since the user invoked `corfu-complete'
          ;; with an explicitly selected candidate!
          (let ((newstr (buffer-substring-no-properties beg end)))
            (when (and (test-completion newstr table pred)
                       (or
                        (not (consp (completion-try-completion
                                     newstr table pred (length newstr)
                                     (completion-metadata newstr table pred))))
                        (equal (completion-boundaries newstr table pred "") '(0 . 0))))
              (corfu--done newstr 'finished))))
      ;; Try to complete the current input string
      (let* ((pt (max 0 (- (point) beg)))
             (str (buffer-substring-no-properties beg end))
             (metadata (completion-metadata (substring str 0 pt) table pred)))
        (pcase (completion-try-completion str table pred pt metadata)
          ('t
           (goto-char end)
           (corfu--done str 'finished))
          (`(,newstr . ,newpt)
           (unless (equal str newstr)
             ;; bug#55205: completion--replace removes properties!
             (completion--replace beg end (concat newstr)))
           (goto-char (+ beg newpt))
           ;; Exit with status 'finished if input is a valid match
           ;; and no further completion is possible.
           (when (and (test-completion newstr table pred)
                      (not (consp (completion-try-completion
                                   newstr table pred newpt
                                   (completion-metadata (substring newstr 0 newpt) table pred)))))
             (corfu--done newstr 'finished))))))))

(defun corfu--insert (status)
  "Insert current candidate, exit with STATUS if non-nil."
  (pcase-let* ((`(,beg ,end . ,_) completion-in-region--data)
               (str (buffer-substring-no-properties beg end)))
    ;; XXX There is a small bug here, depending on interpretation.
    ;; When completing "~/emacs/master/li|/calc" where "|" is the
    ;; cursor, then the candidate only includes the prefix
    ;; "~/emacs/master/lisp/", but not the suffix "/calc". Default
    ;; completion has the same problem when selecting in the
    ;; *Completions* buffer. See bug#48356.
    (setq str (concat corfu--base (substring-no-properties
                                   (nth corfu--index corfu--candidates))))
    ;; bug#55205: completion--replace removes properties!
    (completion--replace beg end (concat str))
    (corfu--goto -1) ;; Reset selection, but continue completion.
    (when status (corfu--done str status)))) ;; Exit with status

(defun corfu--done (str status)
  "Call the `:exit-function' with STR and STATUS and exit completion."
  (let ((exit (plist-get corfu--extra :exit-function)))
    ;; For successfull completions, amalgamate undo operations,
    ;; such that completion can be undone in a single step.
    (undo-amalgamate-change-group corfu--change-group)
    (corfu-quit)
    (when exit (funcall exit str status))))

(defun corfu-insert ()
  "Insert current candidate.
Quit if no candidate is selected."
  (interactive)
  (if (>= corfu--index 0)
      (corfu--insert 'finished)
    (corfu-quit)))

(defun corfu--setup ()
  "Setup Corfu completion state."
  (setq corfu--extra completion-extra-properties)
  (completion-in-region-mode 1)
  (undo-boundary) ;; Necessary to support `corfu-reset'
  (activate-change-group (setq corfu--change-group (prepare-change-group)))
  (setcdr (assq #'completion-in-region-mode minor-mode-overriding-map-alist) corfu-map)
  (add-hook 'pre-command-hook #'corfu--pre-command nil 'local)
  (add-hook 'post-command-hook #'corfu--post-command)
  ;; Disable default post-command handling, since we have our own
  ;; checks in `corfu--post-command'.
  (remove-hook 'post-command-hook #'completion-in-region--postch)
  (let ((sym (make-symbol "corfu--teardown"))
        (buf (current-buffer)))
    (fset sym (lambda ()
                ;; Ensure that the teardown runs in the correct buffer, if still alive.
                (unless completion-in-region-mode
                  (remove-hook 'completion-in-region-mode-hook sym)
                  (with-current-buffer (if (buffer-live-p buf) buf (current-buffer))
                    (corfu--teardown)))))
    (add-hook 'completion-in-region-mode-hook sym)))

(defun corfu--teardown ()
  "Teardown Corfu."
  (corfu--popup-hide)
  (remove-hook 'pre-command-hook #'corfu--pre-command 'local)
  (remove-hook 'post-command-hook #'corfu--post-command)
  (when corfu--preview-ov (delete-overlay corfu--preview-ov))
  (accept-change-group corfu--change-group)
  (mapc #'kill-local-variable corfu--state-vars))

(defun corfu--in-region (&rest args)
  "Corfu completion in region function called with ARGS."
  ;; XXX We can get an endless loop when `completion-in-region-function' is set
  ;; globally to `corfu--in-region'. This should never happen.
  (apply (if (corfu--popup-support-p) #'corfu--in-region-1
           (default-value 'completion-in-region-function))
         args))

(defun corfu--in-region-1 (beg end table &optional pred)
  "Complete in region, see `completion-in-region' for BEG, END, TABLE, PRED."
  (barf-if-buffer-read-only)
  ;; Restart the completion. This can happen for example if C-M-/
  ;; (`dabbrev-completion') is pressed while the Corfu popup is already open.
  (when completion-in-region-mode (corfu-quit))
  (let* ((pt (max 0 (- (point) beg)))
         (str (buffer-substring-no-properties beg end))
         (metadata (completion-metadata (substring str 0 pt) table pred))
         (exit (plist-get completion-extra-properties :exit-function))
         (threshold (completion--cycle-threshold metadata))
         (completion-in-region-mode-predicate
          (or completion-in-region-mode-predicate (lambda () t))))
    (pcase (completion-try-completion str table pred pt metadata)
      ('nil (corfu--message "No match") nil)
      ('t (goto-char end)
          (corfu--message "Sole match")
          (when exit (funcall exit str 'finished))
          t)
      (`(,newstr . ,newpt)
       (let* ((state (corfu--recompute str pt table pred))
              (base (alist-get 'corfu--base state))
              (total (alist-get 'corfu--total state))
              (candidates (alist-get 'corfu--candidates state)))
         (unless (markerp beg) (setq beg (copy-marker beg)))
         (setq end (copy-marker end t)
               completion-in-region--data (list beg end table pred))
         (unless (equal str newstr)
           ;; bug#55205: completion--replace removes properties!
           (completion--replace beg end (concat newstr)))
         (goto-char (+ beg newpt))
         (if (= total 1)
             ;; If completion is finished and cannot be further completed,
             ;; return 'finished. Otherwise setup the Corfu popup.
             (cond
              ((consp (completion-try-completion
                       newstr table pred newpt
                       (completion-metadata newstr table pred)))
               (corfu--setup))
              (exit (funcall exit newstr 'finished)))
           (if (or (= total 0) (not threshold)
                   (and (not (eq threshold t)) (< threshold total)))
               (corfu--setup)
             (corfu--cycle-candidates total candidates (+ (length base) beg) end)
             ;; Do not show Corfu when "trivially" cycling, i.e.,
             ;; when the completion is finished after the candidate.
             (unless (equal (completion-boundaries (car candidates) table pred "")
                            '(0 . 0))
               (corfu--setup)))))
       t))))

(defun corfu--message (&rest msg)
  "Show completion MSG."
  (let (message-log-max) (apply #'message msg)))

(defun corfu--cycle-candidates (total cands beg end)
  "Cycle between TOTAL number of CANDS.
See `completion-in-region' for the arguments BEG, END, TABLE, PRED."
  (let* ((idx 0)
         (map (make-sparse-keymap))
         (replace (lambda ()
                    (interactive)
                    ;; bug#55205: completion--replace removes properties!
                    (completion--replace beg end (concat (nth idx cands)))
                    (corfu--message "Cycling %d/%d..." (1+ idx) total)
                    (setq idx (mod (1+ idx) total))
                    (set-transient-map map))))
    (define-key map [remap completion-at-point] replace)
    (define-key map [remap corfu-complete] replace)
    (define-key map (vector last-command-event) replace)
    (funcall replace)))

(defun corfu--auto-complete-deferred (&optional tick)
  "Initiate auto completion if TICK did not change."
  (setq corfu--auto-timer nil)
  (when (and (not completion-in-region-mode)
             (or (not tick) (equal tick (corfu--auto-tick))))
    (pcase (while-no-input ;; Interruptible capf query
             (run-hook-wrapped 'completion-at-point-functions #'corfu--capf-wrapper))
      (`(,fun ,beg ,end ,table . ,plist)
       (let ((completion-in-region-mode-predicate
              (lambda () (eq beg (car-safe (funcall fun)))))
             (completion-extra-properties plist))
         (setq completion-in-region--data
               (list (if (markerp beg) beg (copy-marker beg))
                     (copy-marker end t)
                     table
                     (plist-get plist :predicate)))
         (corfu--setup)
         (corfu--exhibit 'auto))))))

(defun corfu--auto-post-command ()
  "Post command hook which initiates auto completion."
  (when corfu--auto-timer
    (cancel-timer corfu--auto-timer)
    (setq corfu--auto-timer nil))
  (when (and (not completion-in-region-mode)
             (not defining-kbd-macro)
             (not buffer-read-only)
             (corfu--match-symbol-p corfu-auto-commands this-command)
             (corfu--popup-support-p))
    (if (<= corfu-auto-delay 0)
        (corfu--auto-complete-deferred)
      ;; NOTE: Do not use idle timer since this leads to unacceptable slowdowns,
      ;; in particular if flyspell-mode is enabled.
      (setq corfu--auto-timer
            (run-at-time corfu-auto-delay nil
                         #'corfu--auto-complete-deferred (corfu--auto-tick))))))

(defun corfu--auto-tick ()
  "Return the current tick/status of the buffer.
Auto completion is only performed if the tick did not change."
  (list (current-buffer) (buffer-chars-modified-tick) (point)))

;;;###autoload
(define-minor-mode corfu-mode
  "Completion Overlay Region FUnction."
  :global nil :group 'corfu
  (cond
   (corfu-mode
    ;; FIXME: Install advice which fixes `completion--capf-wrapper', such that
    ;; it respects the completion styles for non-exclusive capfs. See FIXME in
    ;; the `completion--capf-wrapper' function in minibuffer.el, where the
    ;; issue has been mentioned. We never uninstall this advice since the
    ;; advice is active *globally*.
    (advice-add #'completion--capf-wrapper :around #'corfu--capf-wrapper-advice)
    (advice-add #'eldoc-display-message-no-interference-p :before-while #'corfu--allow-eldoc)
    (and corfu-auto (add-hook 'post-command-hook #'corfu--auto-post-command nil 'local))
    (setq-local completion-in-region-function #'corfu--in-region))
   (t
    (remove-hook 'post-command-hook #'corfu--auto-post-command 'local)
    (kill-local-variable 'completion-in-region-function))))

(defun corfu--capf-wrapper (fun &optional prefix)
  "Wrapper for `completion-at-point' FUN.
The wrapper determines if the capf is applicable at the current position
and performs sanity checking on the returned result. PREFIX is a prefix
length override, set to t for manual completion."
  (pcase (funcall fun)
    ((and res `(,beg ,end ,table . ,plist))
     (and (integer-or-marker-p beg) ;; Valid capf result
          (<= beg (point) end)      ;; Sanity checking
          ;; When auto completing, check the prefix length!
          (let ((len (or prefix
                         (plist-get plist :company-prefix-length)
                         (- (point) beg))))
            (or (eq len t) (>= len corfu-auto-prefix)))
          ;; For non-exclusive capfs, check for valid completion.
          (or (not (eq 'no (plist-get plist :exclusive)))
              (let* ((str (buffer-substring-no-properties beg end))
                     (pt (- (point) beg))
                     (pred (plist-get plist :predicate))
                     (md (completion-metadata (substring str 0 pt) table pred)))
                ;; We use `completion-try-completion' to check if there are
                ;; completions. The upstream `completion--capf-wrapper' uses
                ;; `try-completion' which is incorrect since it only checks for
                ;; prefix completions.
                (completion-try-completion str table pred pt md)))
          (cons fun res)))))

(defun corfu--capf-wrapper-advice (orig fun which)
  "Around advice for `completion--capf-wrapper'.
The ORIG function takes the FUN and WHICH arguments."
  (if corfu-mode (corfu--capf-wrapper fun t) (funcall orig fun which)))

;;;###autoload
(define-globalized-minor-mode global-corfu-mode corfu-mode corfu--on :group 'corfu)

(defun corfu--on ()
  "Turn `corfu-mode' on."
  (unless (or noninteractive
              (eq (aref (buffer-name) 0) ?\s)
              (memq major-mode corfu-excluded-modes))
    (corfu-mode 1)))

(defun corfu--allow-eldoc ()
  "Return non-nil if Corfu is currently not active."
  (not (and corfu-mode completion-in-region-mode)))

;; Emacs 28: Do not show Corfu commands with M-X
(dolist (sym '(corfu-next corfu-previous corfu-first corfu-last corfu-quit corfu-reset
               corfu-complete corfu-insert corfu-scroll-up corfu-scroll-down
               corfu-insert-separator))
  (put sym 'completion-predicate #'ignore))

(provide 'corfu)
;;; corfu.el ends here
