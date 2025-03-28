;;; corfu.el --- COmpletion in Region FUnction -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 2.0
;; Package-Requires: ((emacs "28.1") (compat "30"))
;; URL: https://github.com/minad/corfu
;; Keywords: abbrev, convenience, matching, completion, text

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

;; Corfu enhances in-buffer completion with a small completion popup.
;; The current candidates are shown in a popup below or above the
;; point.  The candidates can be selected by moving up and down.
;; Corfu is the minimalistic in-buffer completion counterpart of the
;; Vertico minibuffer UI.

;;; Code:

(require 'compat)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup corfu nil
  "COmpletion in Region FUnction."
  :link '(info-link :tag "Info Manual" "(corfu)")
  :link '(url-link :tag "Website" "https://github.com/minad/corfu")
  :link '(url-link :tag "Wiki" "https://github.com/minad/corfu/wiki")
  :link '(emacs-library-link :tag "Library Source" "corfu.el")
  :group 'convenience
  :group 'tools
  :group 'matching
  :prefix "corfu-")

(defcustom corfu-count 10
  "Maximal number of candidates to show."
  :type 'natnum)

(defcustom corfu-scroll-margin 2
  "Number of lines at the top and bottom when scrolling.
The value should lie between 0 and corfu-count/2."
  :type 'natnum)

(defcustom corfu-min-width 15
  "Popup minimum width in characters."
  :type 'natnum)

(defcustom corfu-max-width 100
  "Popup maximum width in characters."
  :type 'natnum)

(defcustom corfu-cycle nil
  "Enable cycling for `corfu-next' and `corfu-previous'."
  :type 'boolean)

(defcustom corfu-on-exact-match 'insert
  "Configure how a single exact match should be handled.
- nil: No special handling, continue completion.
- insert: Insert candidate, quit and call the `:exit-function'.
- quit: Quit completion without further action.
- show: Initiate completion even for a single match only."
  :type '(choice (const insert) (const show) (const quit) (const nil)))

(defcustom corfu-continue-commands
  '(ignore universal-argument universal-argument-more digit-argument
    "\\`corfu-" "\\`scroll-other-window")
  "Continue Corfu completion after executing these commands.
The list can container either command symbols or regular expressions."
  :type '(repeat (choice regexp symbol)))

(defcustom corfu-preview-current 'insert
  "Preview currently selected candidate.
If the variable has the value `insert', the candidate is automatically
inserted on further input."
  :type '(choice boolean (const insert)))

(defcustom corfu-preselect 'valid
  "Configure if the prompt or first candidate is preselected.
- prompt: Always select the prompt.
- first: Always select the first candidate.
- valid: Only select the prompt if valid and not equal to the first candidate.
- directory: Like first, but select the prompt if it is a directory."
  :type '(choice (const prompt) (const valid) (const first) (const directory)))

(defcustom corfu-separator ?\s
  "Component separator character.
The character used for separating components in the input.  The presence
of this separator character will inhibit quitting at completion
boundaries, so that any further characters can be entered.  To enter the
first separator character, call `corfu-insert-separator' (bound to M-SPC
by default).  Useful for multi-component completion styles such as
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
argument until an appropriate formatter is found.  The function should
return a formatter function, which takes the candidate string and must
return a string, possibly an icon.  In order to preserve correct popup
alignment, the length and display width of the returned string must
precisely span the same number of characters of the fixed-width popup
font.  For example the kind-icon package returns a string of length 3
with a display width of 3 characters."
  :type 'hook)

(defcustom corfu-sort-function #'corfu-sort-length-alpha
  "Default sorting function.
This function is used if the completion table does not specify a
`display-sort-function'."
  :type `(choice
          (const :tag "No sorting" nil)
          (const :tag "By length and alpha" ,#'corfu-sort-length-alpha)
          (function :tag "Custom function")))

(defcustom corfu-sort-override-function nil
  "Override sort function which overrides the `display-sort-function'.
This function is used even if a completion table specifies its
own sort function."
  :type '(choice (const nil) function))

(defcustom corfu-auto-prefix 3
  "Minimum length of prefix for auto completion.
The completion backend can override this with
:company-prefix-length.  It is *not recommended* to use a small
prefix length (below 2), since this will create high load for
Emacs.  See also `corfu-auto-delay'."
  :type 'natnum)

(defcustom corfu-auto-delay 0.2
  "Delay for auto completion.
It is *not recommended* to use a short delay or even 0, since
this will create high load for Emacs, in particular if executing
the completion backend is costly."
  :type 'float)

(defcustom corfu-auto-commands
  '("self-insert-command\\'" "delete-backward-char\\'" "\\`backward-delete-char"
    c-electric-colon c-electric-lt-gt c-electric-slash c-scope-operator)
  "Commands which initiate auto completion.
The list can container either command symbols or regular expressions."
  :type '(repeat (choice regexp symbol)))

(defcustom corfu-auto nil
  "Enable auto completion.
Auto completion is disabled by default for safety and unobtrusiveness.
Note that auto completion is particularly dangerous in untrusted files
since some completion functions may perform arbitrary code execution,
notably the Emacs built-in `elisp-completion-at-point' .  See also the
settings `corfu-auto-delay', `corfu-auto-prefix' and
`corfu-auto-commands'."
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
     :background "#00415e" :foreground "white" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#c0efff" :foreground "black" :extend t)
    (t :background "blue" :foreground "white" :extend t))
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

(defvar-keymap corfu-mode-map
  :doc "Keymap used when `corfu-mode' is active.")

(defvar-keymap corfu-map
  :doc "Keymap used when popup is shown."
  "<remap> <move-beginning-of-line>" #'corfu-prompt-beginning
  "<remap> <move-end-of-line>" #'corfu-prompt-end
  "<remap> <beginning-of-buffer>" #'corfu-first
  "<remap> <end-of-buffer>" #'corfu-last
  "<remap> <scroll-down-command>" #'corfu-scroll-down
  "<remap> <scroll-up-command>" #'corfu-scroll-up
  "<remap> <next-line>" #'corfu-next
  "<remap> <previous-line>" #'corfu-previous
  "<remap> <completion-at-point>" #'corfu-complete
  "<remap> <keyboard-escape-quit>" #'corfu-reset
  "<down>" #'corfu-next
  "<up>" #'corfu-previous
  "M-n" #'corfu-next
  "M-p" #'corfu-previous
  "C-g" #'corfu-quit
  "RET" #'corfu-insert
  "TAB" #'corfu-complete
  "M-TAB" #'corfu-expand
  "M-g" 'corfu-info-location
  "M-h" 'corfu-info-documentation
  "M-SPC" #'corfu-insert-separator)

(defvar corfu--auto-timer (timer-create)
  "Auto completion timer.")

(defvar corfu--candidates nil
  "List of candidates.")

(defvar corfu--metadata nil
  "Completion metadata.")

(defvar corfu--base ""
  "Base string, which is concatenated with the candidate.")

(defvar corfu--total 0
  "Length of the candidate list `corfu--candidates'.")

(defvar corfu--hilit #'identity
  "Lazy candidate highlighting function.")

(defvar corfu--index -1
  "Index of current candidate or negative for prompt selection.")

(defvar corfu--preselect -1
  "Index of preselected candidate, negative for prompt selection.")

(defvar corfu--scroll 0
  "Scroll position.")

(defvar corfu--input nil
  "Cons of last prompt contents and point.")

(defvar corfu--preview-ov nil
  "Current candidate overlay.")

(defvar corfu--change-group nil
  "Undo change group.")

(defvar corfu--frame nil
  "Popup frame.")

(defvar corfu--width 0
  "Popup width of current completion to reduce width fluctuations.")

(defconst corfu--initial-state
  (mapcar
   (lambda (k) (cons k (symbol-value k)))
   '(corfu--base
     corfu--candidates
     corfu--hilit
     corfu--index
     corfu--preselect
     corfu--scroll
     corfu--input
     corfu--total
     corfu--preview-ov
     corfu--change-group
     corfu--metadata
     corfu--width))
  "Initial Corfu state.")

(defvar corfu--frame-parameters
  '((no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width . t)
    (min-height . t)
    (border-width . 0)
    (outer-border-width . 0)
    (internal-border-width . 1)
    (child-frame-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (tab-bar-lines-keep-state . t)
    (no-other-frame . t)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t))
  "Default child frame parameters.")

(defvar corfu--buffer-parameters
  '((mode-line-format . nil)
    (header-line-format . nil)
    (tab-line-format . nil)
    (tab-bar-format . nil)
    (frame-title-format . "")
    (truncate-lines . t)
    (cursor-in-non-selected-windows . nil)
    (cursor-type . nil)
    (show-trailing-whitespace . nil)
    (display-line-numbers . nil)
    (left-fringe-width . 0)
    (right-fringe-width . 0)
    (left-margin-width . 0)
    (right-margin-width . 0)
    (fringes-outside-margins . 0)
    (fringe-indicator-alist (continuation) (truncation))
    (indicate-empty-lines . nil)
    (indicate-buffer-boundaries . nil)
    (buffer-read-only . t))
  "Default child frame buffer parameters.")

(defvar corfu--mouse-ignore-map
  (let ((map (define-keymap "<touchscreen-begin>" #'ignore)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (keymap-set map (format "<%s-%s>" k (1+ i)) #'ignore)))
    map)
  "Ignore all mouse clicks.")

(defun corfu--replace (beg end str)
  "Replace range between BEG and END with STR."
  (unless (equal str (buffer-substring-no-properties beg end))
    ;; bug#55205: completion--replace removed properties as an unwanted
    ;; side-effect.  We also don't want to leave text properties.
    (completion--replace beg end (substring-no-properties str))))

(defun corfu--capf-wrapper (fun &optional prefix)
  "Wrapper for `completion-at-point' FUN.
The wrapper determines if the Capf is applicable at the current position
and performs sanity checking on the returned result.  For non-exclusive
Capfs, the wrapper checks if the current input can be completed.  PREFIX
is a prefix length override, which is t for manual completion."
  (pcase (funcall fun)
    ((and res `(,beg ,end ,table . ,plist))
     (and (integer-or-marker-p beg) ;; Valid Capf result
          (<= beg (point) end)      ;; Sanity checking
          ;; When auto completing, check the prefix length!
          (let ((len (or prefix
                         (plist-get plist :company-prefix-length)
                         (- (point) beg))))
            (or (eq len t) (>= len corfu-auto-prefix)))
          ;; For non-exclusive Capfs, check for valid completion.
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

(defvar x-gtk-resize-child-frames) ;; Not present on non-gtk builds
(defvar corfu--gtk-resize-child-frames
  (let ((case-fold-search t))
     ;; XXX HACK to fix resizing on gtk3/gnome taken from posframe.el
     ;; More information:
     ;; * https://github.com/minad/corfu/issues/17
     ;; * https://gitlab.gnome.org/GNOME/mutter/-/issues/840
     ;; * https://lists.gnu.org/archive/html/emacs-devel/2020-02/msg00001.html
    (and (string-match-p "gtk3" system-configuration-features)
         (string-match-p "gnome\\|cinnamon"
                         (or (getenv "XDG_CURRENT_DESKTOP")
                             (getenv "DESKTOP_SESSION") ""))
         'resize-mode)))

;; Function adapted from posframe.el by tumashu
(defun corfu--make-frame (frame x y width height)
  "Show current buffer in child frame at X/Y with WIDTH/HEIGHT.
FRAME is the existing frame."
  (when-let (((frame-live-p frame))
             (timer (frame-parameter frame 'corfu--hide-timer)))
    (cancel-timer timer)
    (set-frame-parameter frame 'corfu--hide-timer nil))
  (let* ((window-min-height 1)
         (window-min-width 1)
         (inhibit-redisplay t)
         (x-gtk-resize-child-frames corfu--gtk-resize-child-frames)
         (before-make-frame-hook)
         (after-make-frame-functions)
         (parent (window-frame)))
    (unless (and (frame-live-p frame)
                 (eq (frame-parent frame)
                     (and (not (bound-and-true-p exwm--connection)) parent))
                 ;; If there is more than one window, `frame-root-window' may
                 ;; return nil.  Recreate the frame in this case.
                 (window-live-p (frame-root-window frame)))
      (when frame (delete-frame frame))
      (setq frame (make-frame
                   `((parent-frame . ,parent)
                     (minibuffer . ,(minibuffer-window parent))
                     (width . 0) (height . 0) (visibility . nil)
                     (right-fringe . ,right-fringe-width)
                     (left-fringe . ,left-fringe-width)
                     ,@corfu--frame-parameters))))
    ;; XXX HACK Setting the same frame-parameter/face-background is not a nop.
    ;; Check before applying the setting. Without the check, the frame flickers
    ;; on Mac. We have to apply the face background before adjusting the frame
    ;; parameter, otherwise the border is not updated.
    (let ((new (face-attribute 'corfu-border :background nil 'default)))
      (unless (equal (face-attribute 'internal-border :background frame 'default) new)
        (set-face-background 'internal-border new frame))
      ;; XXX The Emacs Mac Port does not support `internal-border', we also have
      ;; to set `child-frame-border'.
      (unless (or (not (facep 'child-frame-border))
                  (equal (face-attribute 'child-frame-border :background frame 'default) new))
        (set-face-background 'child-frame-border new frame)))
    ;; Reset frame parameters if they changed.  For example `tool-bar-mode'
    ;; overrides the parameter `tool-bar-lines' for every frame, including child
    ;; frames.  The child frame API is a pleasure to work with.  It is full of
    ;; lovely surprises.
    (let* ((win (frame-root-window frame))
           (is (frame-parameters frame))
           (should `((background-color
                      . ,(face-attribute 'corfu-default :background nil 'default))
                     (font . ,(frame-parameter parent 'font))
                     (right-fringe . ,right-fringe-width)
                     (left-fringe . ,left-fringe-width)
                     ,@corfu--frame-parameters))
           (diff (cl-loop for p in should for (k . v) = p
                          unless (equal (alist-get k is) v) collect p)))
      (when diff (modify-frame-parameters frame diff))
      ;; XXX HACK: `set-window-buffer' must be called to force fringe update.
      (when (or diff (eq (window-buffer win) (current-buffer)))
        (set-window-buffer win (current-buffer)))
      ;; Disallow selection of root window (gh:minad/corfu#63)
      (set-window-parameter win 'no-delete-other-windows t)
      (set-window-parameter win 'no-other-window t)
      ;; Mark window as dedicated to prevent frame reuse (gh:minad/corfu#60)
      (set-window-dedicated-p win t))
    (redirect-frame-focus frame parent)
    (set-frame-size frame width height t)
    (pcase-let ((`(,px . ,py) (frame-position frame)))
      (unless (and (= x px) (= y py))
        (set-frame-position frame x y))))
  (make-frame-visible frame)
  ;; Unparent child frame if EXWM is used, otherwise EXWM buffers are drawn on
  ;; top of the Corfu child frame.
  (when (and (bound-and-true-p exwm--connection) (frame-parent frame))
    (set-frame-parameter frame 'parent-frame nil))
  frame)

(defun corfu--hide-frame-deferred (frame)
  "Deferred hiding of child FRAME."
  (when (and (frame-live-p frame) (frame-visible-p frame))
    (set-frame-parameter frame 'corfu--hide-timer nil)
    (make-frame-invisible frame)
    (with-current-buffer (window-buffer (frame-root-window frame))
      (with-silent-modifications
        (delete-region (point-min) (point-max))))))

(defun corfu--hide-frame (frame)
  "Hide child FRAME."
  (when (and (frame-live-p frame) (frame-visible-p frame)
             (not (frame-parameter frame 'corfu--hide-timer)))
    (set-frame-parameter frame 'corfu--hide-timer
                         (run-at-time 0 nil #'corfu--hide-frame-deferred frame))))

(defun corfu--move-to-front (elem list)
  "Move ELEM to front of LIST."
  ;; In contrast to Vertico, this function handles duplicates. See also the
  ;; special deduplication function `corfu--delete-dups' based on
  ;; `equal-including-properties'
  (nconc (cl-loop for x in list if (equal x elem) collect x)
         (delete elem list)))

(defun corfu--filter-completions (&rest args)
  "Compute all completions for ARGS with lazy highlighting."
  (dlet ((completion-lazy-hilit t) (completion-lazy-hilit-fn nil))
    (static-if (>= emacs-major-version 30)
        (cons (apply #'completion-all-completions args) completion-lazy-hilit-fn)
      (cl-letf* ((orig-pcm (symbol-function #'completion-pcm--hilit-commonality))
                 (orig-flex (symbol-function #'completion-flex-all-completions))
                 ((symbol-function #'completion-flex-all-completions)
                  (lambda (&rest args)
                    ;; Unfortunately for flex we have to undo the lazy highlighting, since flex uses
                    ;; the completion-score for sorting, which is applied during highlighting.
                    (cl-letf (((symbol-function #'completion-pcm--hilit-commonality) orig-pcm))
                      (apply orig-flex args))))
                 ((symbol-function #'completion-pcm--hilit-commonality)
                  (lambda (pattern cands)
                    (setq completion-lazy-hilit-fn
                          (lambda (x)
                            ;; `completion-pcm--hilit-commonality' sometimes throws an internal error
                            ;; for example when entering "/sudo:://u".
                            (condition-case nil
                                (car (completion-pcm--hilit-commonality pattern (list x)))
                              (t x))))
                    cands))
                 ((symbol-function #'completion-hilit-commonality)
                  (lambda (cands prefix &optional base)
                    (setq completion-lazy-hilit-fn
                          (lambda (x) (car (completion-hilit-commonality (list x) prefix base))))
                    (and cands (nconc cands base)))))
        (cons (apply #'completion-all-completions args) completion-lazy-hilit-fn)))))

(defsubst corfu--length-string< (x y)
  "Sorting predicate which compares X and Y first by length then by `string<'."
  (or (< (length x) (length y)) (and (= (length x) (length y)) (string< x y))))

(defmacro corfu--partition! (list form)
  "Evaluate FORM for every element and partition LIST."
  (cl-with-gensyms (head1 head2 tail1 tail2)
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

(defun corfu--move-prefix-candidates-to-front (field cands)
  "Move CANDS which match prefix of FIELD to the beginning."
  (let* ((word (substring field 0
                          (seq-position field corfu-separator)))
         (len (length word)))
    (corfu--partition!
     cands
     (and (>= (length it) len)
          (eq t (compare-strings word 0 len it 0 len
                                 completion-ignore-case))))))

;; bug#6581: `equal-including-properties' uses `eq' for properties until 29.1.
;; Approximate by comparing `text-properties-at' position 0.
(defalias 'corfu--equal-including-properties
  (static-if (< emacs-major-version 29)
      (lambda (x y)
        (and (equal x y)
             (equal (text-properties-at 0 x) (text-properties-at 0 y))))
    #'equal-including-properties))

(defun corfu--delete-dups (list)
  "Delete `equal-including-properties' consecutive duplicates from LIST."
  (let ((beg list))
    (while (cdr beg)
      (let ((end (cdr beg)))
        (while (equal (car beg) (car end)) (pop end))
        ;; The deduplication is quadratic in the number of duplicates.  We can
        ;; avoid the quadratic complexity with a hash table which takes
        ;; properties into account (available since Emacs 28).
        (while (not (eq beg end))
          (let ((dup beg))
            (while (not (eq (cdr dup) end))
              (if (corfu--equal-including-properties (car beg) (cadr dup))
                  (setcdr dup (cddr dup))
                (pop dup))))
          (pop beg)))))
  list)

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
               ;; bug#47678: `completion-boundaries' fails for `partial-completion'
               ;; if the cursor is moved before the slashes of "~//".
               ;; See also vertico.el which has the same issue.
               (bounds (condition-case nil
                           (completion-boundaries before table pred after)
                         (t (cons 0 (length after)))))
               (field (substring str (car bounds) (+ pt (cdr bounds))))
               (completing-file (eq (corfu--metadata-get 'category) 'file))
               (`(,all . ,hl) (corfu--filter-completions str table pred pt corfu--metadata))
               (base (or (when-let ((z (last all))) (prog1 (cdr z) (setcdr z nil))) 0))
               (corfu--base (substring str 0 base))
               (pre nil))
    ;; Filter the ignored file extensions. We cannot use modified predicate for
    ;; this filtering, since this breaks the special casing in the
    ;; `completion-file-name-table' for `file-exists-p' and `file-directory-p'.
    (when completing-file (setq all (completion-pcm--filename-try-filter all)))
    ;; Sort using the `display-sort-function' or the Corfu sort functions, and
    ;; delete duplicates with respect to `equal-including-properties'.  This is
    ;; a deviation from the Vertico completion UI with more aggressive
    ;; deduplication, where candidates are compared with `equal'.  Corfu
    ;; preserves candidates which differ in their text properties.  Corfu tries
    ;; to preserve text properties as much as possible, when calling the
    ;; `:exit-function' to help Capfs with candidate disambiguation.  This
    ;; matters in particular for Lsp backends, which produce duplicates for
    ;; overloaded methods.
    (setq all (corfu--delete-dups (funcall (or (corfu--sort-function) #'identity) all))
          all (corfu--move-prefix-candidates-to-front field all))
    (when (and completing-file (not (string-suffix-p "/" field)))
      (setq all (corfu--move-to-front (concat field "/") all)))
    (setq all (corfu--move-to-front field all)
          pre (if (or (eq corfu-preselect 'prompt) (not all)
                      (and completing-file (eq corfu-preselect 'directory)
                           (= (length corfu--base) (length str))
                           (test-completion str table pred))
                      (and (eq corfu-preselect 'valid)
                           (not (equal field (car all)))
                           (not (and completing-file (equal (concat field "/") (car all))))
                           (test-completion str table pred)))
                  -1 0))
    `((corfu--base . ,corfu--base)
      (corfu--metadata . ,corfu--metadata)
      (corfu--candidates . ,all)
      (corfu--total . ,(length all))
      (corfu--hilit . ,(or hl #'identity))
      (corfu--preselect . ,pre)
      (corfu--index . ,(or (and (>= corfu--index 0) (/= corfu--index corfu--preselect)
                                (seq-position all (nth corfu--index corfu--candidates)))
                           pre)))))

(defun corfu--update (&optional interruptible)
  "Update state, optionally INTERRUPTIBLE."
  (pcase-let* ((`(,beg ,end ,table ,pred . ,_) completion-in-region--data)
               (pt (- (point) beg))
               (str (buffer-substring-no-properties beg end))
               (input (cons str pt)))
    (unless (equal corfu--input input)
      ;; Redisplay such that the input is immediately shown before the expensive
      ;; candidate recomputation (gh:minad/corfu#48). See also corresponding
      ;; issue gh:minad/vertico#89.
      (when interruptible (redisplay))
      ;; Bind non-essential=t to prevent Tramp from opening new connections,
      ;; without the user explicitly requesting it via M-TAB.
      (pcase (let ((non-essential t))
               ;; XXX Guard against errors during candidate generation.
               ;; bug#61274: `dabbrev-capf' signals errors.
               (condition-case err
                   (if interruptible
                       (while-no-input (corfu--recompute str pt table pred))
                     (corfu--recompute str pt table pred))
                 (error
                  (message "Corfu completion error: %s" (error-message-string err))
                  t)))
        ('nil (keyboard-quit))
        ((and state (pred consp))
         (setq corfu--input input)
         (dolist (s state) (set (car s) (cdr s))))))
    input))

(defun corfu--match-symbol-p (pattern sym)
  "Return non-nil if SYM is matching an element of the PATTERN list."
  (cl-loop with case-fold-search = nil
           for x in (and (symbolp sym) pattern)
           thereis (if (symbolp x)
                       (eq sym x)
                     (string-match-p x (symbol-name sym)))))

(defun corfu--metadata-get (prop)
  "Return PROP from completion metadata."
  ;; Marginalia and various icon packages advise `completion-metadata-get' to
  ;; inject their annotations, but are meant only for minibuffer completion.
  ;; Therefore call `completion-metadata-get' without advices here.
  (let ((completion-extra-properties (nth 4 completion-in-region--data)))
    (funcall (advice--cd*r (symbol-function (compat-function completion-metadata-get)))
             corfu--metadata prop)))

(defun corfu--format-candidates (cands)
  "Format annotated CANDS."
  (cl-loop for c in cands do
           (cl-loop for s in-ref c do
                    (setf s (replace-regexp-in-string "[ \t]*\n[ \t]*" " " s))))
  (let* ((cw (cl-loop for x in cands maximize (string-width (car x))))
         (pw (cl-loop for x in cands maximize (string-width (cadr x))))
         (sw (cl-loop for x in cands maximize (string-width (caddr x))))
         (width (min (max corfu--width corfu-min-width (+ pw cw sw))
                     ;; -4 because of margins and some additional safety
                     corfu-max-width (- (frame-width) 4)))
         (trunc (not (display-graphic-p))))
    (setq corfu--width width)
    (list pw width
          (cl-loop
           for (cand prefix suffix) in cands collect
           (let ((s (concat
                     prefix (make-string (- pw (string-width prefix)) ?\s) cand
                     (when (> sw 0)
                       (make-string (max 0 (- width pw (string-width cand)
                                              (string-width suffix)))
                                    ?\s))
                     suffix)))
             (if trunc (truncate-string-to-width s width) s))))))

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
               (`(,mf . ,acands) (corfu--affixate
                                  (cl-loop repeat corfu-count
                                           for c in (nthcdr corfu--scroll corfu--candidates)
                                           collect (funcall corfu--hilit (substring c)))))
               (`(,pw ,width ,fcands) (corfu--format-candidates acands))
               ;; Disable the left margin if a margin formatter is active.
               (corfu-left-margin-width (if mf 0 corfu-left-margin-width)))
    ;; Nonlinearity at the end and the beginning
    (when (/= corfu--scroll 0)
      (setq lo (max 1 lo)))
    (when (/= last corfu--total)
      (setq lo (min (- corfu-count bar 2) lo)))
    (corfu--popup-show pos pw width fcands (- corfu--index corfu--scroll)
                       (and (> corfu--total corfu-count) lo) bar)))

(defun corfu--range-valid-p ()
  "Check the completion range, return non-nil if valid."
  (pcase-let ((buf (current-buffer))
              (pt (point))
              (`(,beg ,end . ,_) completion-in-region--data))
    (and beg end
         (eq buf (marker-buffer beg)) (eq buf (window-buffer))
         (<= beg pt end)
         (save-excursion (goto-char beg) (<= (pos-bol) pt (pos-eol))))))

(defun corfu--continue-p ()
  "Check if completion should continue after a command.
Corfu bails out if the current buffer changed unexpectedly or if
point moved out of range, see `corfu--range-valid-p'.  Also the
input must satisfy the `completion-in-region-mode--predicate' and
the last command must be listed in `corfu-continue-commands'."
  (and (corfu--range-valid-p)
       ;; We keep Corfu alive if a `overriding-terminal-local-map' is
       ;; installed, e.g., the `universal-argument-map'. It would be good to
       ;; think about a better criterion instead. Unfortunately relying on
       ;; `this-command' alone is insufficient, since the value of
       ;; `this-command' gets clobbered in the case of transient keymaps.
       (or overriding-terminal-local-map
           ;; Check if it is an explicitly listed continue command
           (corfu--match-symbol-p corfu-continue-commands this-command)
           (pcase-let ((`(,beg ,end . ,_) completion-in-region--data))
             (and (or (not corfu--input) (< beg end)) ;; Check for empty input
                  (or (not corfu-quit-at-boundary) ;; Check separator or predicate
                      (and (eq corfu-quit-at-boundary 'separator)
                           (or (eq this-command #'corfu-insert-separator)
                               ;; with separator, any further chars allowed
                               (seq-contains-p (car corfu--input) corfu-separator)))
                      (funcall completion-in-region-mode--predicate)))))))

(defun corfu--preview-current-p ()
  "Return t if the selected candidate is previewed."
  (and corfu-preview-current (>= corfu--index 0) (/= corfu--index corfu--preselect)))

(defun corfu--preview-current (beg end)
  "Show current candidate as overlay given BEG and END."
  (when (corfu--preview-current-p)
    (corfu--preview-delete)
    (setq beg (+ beg (length corfu--base))
          corfu--preview-ov (make-overlay beg end nil))
    (overlay-put corfu--preview-ov 'priority 1000)
    (overlay-put corfu--preview-ov 'window (selected-window))
    (overlay-put corfu--preview-ov (if (= beg end) 'after-string 'display)
                 (substring-no-properties (nth corfu--index corfu--candidates)))))

(defun corfu--preview-delete ()
  "Delete the preview overlay."
  (when corfu--preview-ov
    (delete-overlay corfu--preview-ov)
    (setq corfu--preview-ov nil)))

(defun corfu--window-change (_)
  "Window and buffer change hook which quits Corfu."
  (unless (corfu--range-valid-p)
    (corfu-quit)))

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

(defun corfu--exit-function (str status cands)
  "Call the `:exit-function' with STR and STATUS.
Lookup STR in CANDS to restore text properties."
  (when-let ((exit (plist-get completion-extra-properties :exit-function)))
    (funcall exit (or (car (member str cands)) str) status)))

(defun corfu--done (str status cands)
  "Exit completion and call the exit function with STR and STATUS.
Lookup STR in CANDS to restore text properties."
  (let ((completion-extra-properties (nth 4 completion-in-region--data)))
    ;; For successful completions, amalgamate undo operations,
    ;; such that completion can be undone in a single step.
    (undo-amalgamate-change-group corfu--change-group)
    (corfu-quit)
    (corfu--exit-function str status cands)))

(defun corfu--setup (beg end table pred)
  "Setup Corfu completion state.
See `completion-in-region' for the arguments BEG, END, TABLE, PRED."
  (setq beg (if (markerp beg) beg (copy-marker beg))
        end (if (and (markerp end) (marker-insertion-type end)) end (copy-marker end t))
        completion-in-region--data (list beg end table pred completion-extra-properties))
  (completion-in-region-mode 1)
  (activate-change-group (setq corfu--change-group (prepare-change-group)))
  (setcdr (assq #'completion-in-region-mode minor-mode-overriding-map-alist) corfu-map)
  (add-hook 'pre-command-hook #'corfu--prepare nil 'local)
  (add-hook 'window-selection-change-functions #'corfu--window-change nil 'local)
  (add-hook 'window-buffer-change-functions #'corfu--window-change nil 'local)
  (add-hook 'post-command-hook #'corfu--post-command)
  ;; Disable default post-command handling, since we have our own
  ;; checks in `corfu--post-command'.
  (remove-hook 'post-command-hook #'completion-in-region--postch)
  (let ((sym (make-symbol "corfu--teardown"))
        (buf (current-buffer)))
    (fset sym (lambda ()
                ;; Ensure that the tear-down runs in the correct buffer, if still alive.
                (unless completion-in-region-mode
                  (remove-hook 'completion-in-region-mode-hook sym)
                  (corfu--teardown buf))))
    (add-hook 'completion-in-region-mode-hook sym)))

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
         (threshold (completion--cycle-threshold metadata))
         (completion-in-region-mode-predicate
          (or completion-in-region-mode-predicate #'always)))
    (pcase (completion-try-completion str table pred pt metadata)
      ('nil (corfu--message "No match") nil)
      ('t (goto-char end)
          (corfu--message "Sole match")
          (if (eq corfu-on-exact-match 'show)
              (corfu--setup beg end table pred)
            (corfu--exit-function
             str 'finished
             (alist-get 'corfu--candidates (corfu--recompute str pt table pred))))
          t)
      (`(,newstr . ,newpt)
       (setq beg (if (markerp beg) beg (copy-marker beg))
             end (copy-marker end t))
       (corfu--replace beg end newstr)
       (goto-char (+ beg newpt))
       (let* ((state (corfu--recompute newstr newpt table pred))
              (base (alist-get 'corfu--base state))
              (total (alist-get 'corfu--total state))
              (cands (alist-get 'corfu--candidates state)))
         (cond
          ((<= total 1)
           ;; If completion is finished and cannot be extended further and
           ;; `corfu-on-exact-match' is not 'show, return 'finished.  Otherwise
           ;; setup the popup.
           (if (and (= total 1)
                    (or (eq corfu-on-exact-match 'show)
                        (consp (completion-try-completion
                                newstr table pred newpt
                                (completion-metadata newstr table pred)))))
               (corfu--setup beg end table pred)
             (corfu--exit-function newstr 'finished cands)))
          ;; Too many candidates for cycling -> Setup popup.
          ((or (not threshold) (and (not (eq threshold t)) (< threshold total)))
           (corfu--setup beg end table pred))
          (t
           ;; Cycle through candidates.
           (corfu--cycle-candidates total cands (+ (length base) beg) end)
           ;; Do not show Corfu when completion is finished after the candidate.
           (unless (equal (completion-boundaries (car cands) table pred "") '(0 . 0))
             (corfu--setup beg end table pred)))))
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
                    (corfu--replace beg end (nth idx cands))
                    (corfu--message "Cycling %d/%d..." (1+ idx) total)
                    (setq idx (mod (1+ idx) total))
                    (set-transient-map map))))
    (define-key map [remap completion-at-point] replace)
    (define-key map [remap corfu-complete] replace)
    (define-key map (vector last-command-event) replace)
    (funcall replace)))

(defun corfu--auto-complete-deferred (&optional tick)
  "Initiate auto completion if TICK did not change."
  (when (and (not completion-in-region-mode)
             (or (not tick) (equal tick (corfu--auto-tick))))
    (pcase (while-no-input ;; Interruptible Capf query
             (run-hook-wrapped 'completion-at-point-functions #'corfu--capf-wrapper))
      (`(,fun ,beg ,end ,table . ,plist)
       (let ((completion-in-region-mode-predicate
              (lambda ()
                (when-let ((newbeg (car-safe (funcall fun))))
                  (= newbeg beg))))
             (completion-extra-properties plist))
         (corfu--setup beg end table (plist-get plist :predicate))
         (corfu--exhibit 'auto))))))

(defun corfu--auto-post-command ()
  "Post command hook which initiates auto completion."
  (cancel-timer corfu--auto-timer)
  (when (and (not completion-in-region-mode)
             (not defining-kbd-macro)
             (not buffer-read-only)
             (corfu--match-symbol-p corfu-auto-commands this-command)
             (corfu--popup-support-p))
    (if (<= corfu-auto-delay 0)
        (corfu--auto-complete-deferred)
      ;; Do not use `timer-set-idle-time' since this leads to
      ;; unpredictable pauses, in particular with `flyspell-mode'.
      (timer-set-time corfu--auto-timer
                      (timer-relative-time nil corfu-auto-delay))
      (timer-set-function corfu--auto-timer #'corfu--auto-complete-deferred
                          (list (corfu--auto-tick)))
      (timer-activate corfu--auto-timer))))

(defun corfu--auto-tick ()
  "Return the current tick/status of the buffer.
Auto completion is only performed if the tick did not change."
  (list (selected-window) (current-buffer) (buffer-chars-modified-tick) (point)))

(cl-defgeneric corfu--popup-show (pos off width lines &optional curr lo bar)
  "Show LINES as popup at POS - OFF.
WIDTH is the width of the popup.
The current candidate CURR is highlighted.
A scroll bar is displayed from LO to LO+BAR."
  (let ((lh (default-line-height)))
    (with-current-buffer (corfu--make-buffer " *corfu*")
      (let* ((ch (default-line-height))
             (cw (default-font-width))
             ;; bug#74214, bug#37755, bug#37689: Even for larger fringes, fringe
             ;; bitmaps can only have a width between 1 and 16. Therefore we
             ;; restrict the fringe width to 16 pixel. This restriction may
             ;; cause problem on HDPi systems.  Hopefully Emacs will adopt
             ;; larger fringe bitmaps in the future and lift the restriction.
             (ml (min 16 (ceiling (* cw corfu-left-margin-width))))
             (mr (min 16 (ceiling (* cw corfu-right-margin-width))))
             (bw (min mr (ceiling (* cw corfu-bar-width))))
             (fringe (display-graphic-p))
             (marginl (and (not fringe) (propertize " " 'display `(space :width (,ml)))))
             (sbar (if fringe
                       #(" " 0 1 (display (right-fringe corfu--bar corfu--bar)))
                     (concat
                      (propertize " " 'display `(space :align-to (- right (,bw))))
                      (propertize " " 'face 'corfu-bar 'display `(space :width (,bw))))))
             (cbar (if fringe
                       #("  " 0 1 (display (left-fringe corfu--nil corfu-current))
                         1 2 (display (right-fringe corfu--bar corfu--cbar)))
                     sbar))
             (cmargin (and fringe
                           #("  " 0 1 (display (left-fringe corfu--nil corfu-current))
                             1 2 (display (right-fringe corfu--nil corfu-current)))))
             (pos (posn-x-y pos))
             (width (+ (* width cw) (if fringe 0 (+ ml mr))))
             ;; XXX HACK: Minimum popup height must be at least 1 line of the
             ;; parent frame (gh:minad/corfu#261).
             (height (max lh (* (length lines) ch)))
             (edge (window-inside-pixel-edges))
             (border (alist-get 'internal-border-width corfu--frame-parameters))
             (x (max 0 (min (+ (car edge) (- (or (car pos) 0) ml (* cw off) border))
                            (- (frame-pixel-width) width))))
             (yb (+ (cadr edge) (or (cdr pos) 0) lh
                    (static-if (< emacs-major-version 31) (window-tab-line-height) 0)))
             (y (if (> (+ yb (* corfu-count ch) lh lh) (frame-pixel-height))
                    (- yb height lh border border)
                  yb))
             (bmp (logxor (1- (ash 1 mr)) (1- (ash 1 bw)))))
        (setq left-fringe-width (if fringe ml 0) right-fringe-width (if fringe mr 0))
        ;; Define an inverted corfu--bar face
        (unless (equal (and (facep 'corfu--bar) (face-attribute 'corfu--bar :foreground))
                       (face-attribute 'corfu-bar :background))
          (set-face-attribute (make-face 'corfu--bar) nil
                              :foreground (face-attribute 'corfu-bar :background)))
        (unless (or (= right-fringe-width 0) (eq (get 'corfu--bar 'corfu--bmp) bmp))
          (put 'corfu--bar 'corfu--bmp bmp)
          (define-fringe-bitmap 'corfu--bar (vector (lognot bmp)) 1 mr '(top periodic))
          (define-fringe-bitmap 'corfu--nil [0] 1 1)
          ;; Fringe bitmaps require symbol face specification, define internal face.
          (set-face-attribute (make-face 'corfu--cbar) nil
                              :inherit '(corfu--bar corfu-current)))
        (with-silent-modifications
          (delete-region (point-min) (point-max))
          (apply #'insert
           (cl-loop for row from 0 for line in lines collect
                    (let ((str (concat marginl line
                                       (if (and lo (<= lo row (+ lo bar)))
                                           (if (eq row curr) cbar sbar)
                                         (and (eq row curr) cmargin))
                                       "\n")))
                      (when (eq row curr)
                        (add-face-text-property
                         0 (length str) 'corfu-current 'append str))
                      str)))
          (goto-char (point-min)))
        (setq corfu--frame (corfu--make-frame corfu--frame x y width height))))))

(cl-defgeneric corfu--popup-hide ()
  "Hide Corfu popup."
  (corfu--hide-frame corfu--frame))

(cl-defgeneric corfu--popup-support-p ()
  "Return non-nil if child frames are supported."
  (or (display-graphic-p) (featurep 'tty-child-frames)))

(cl-defgeneric corfu--insert (status)
  "Insert current candidate, exit with STATUS if non-nil."
  ;; XXX There is a small bug here, depending on interpretation.
  ;; When completing "~/emacs/master/li|/calc" where "|" is the
  ;; cursor, then the candidate only includes the prefix
  ;; "~/emacs/master/lisp/", but not the suffix "/calc". Default
  ;; completion has the same problem when selecting in the
  ;; *Completions* buffer. See bug#48356.
  (pcase-let* ((`(,beg ,end . ,_) completion-in-region--data)
               (str (concat corfu--base (nth corfu--index corfu--candidates))))
    (corfu--replace beg end str)
    (corfu--goto -1) ;; Reset selection, completion may continue.
    (when status (corfu--done str status nil))
    str))

(cl-defgeneric corfu--affixate (cands)
  "Annotate CANDS with annotation function."
  (let* ((dep (corfu--metadata-get 'company-deprecated))
         (mf (let ((completion-extra-properties (nth 4 completion-in-region--data)))
               (run-hook-with-args-until-success 'corfu-margin-formatters corfu--metadata))))
    (setq cands
          (if-let ((aff (corfu--metadata-get 'affixation-function)))
              (funcall aff cands)
            (if-let ((ann (corfu--metadata-get 'annotation-function)))
                (cl-loop for cand in cands collect
                         (let ((suff (or (funcall ann cand) "")))
                           ;; The default completion UI adds the
                           ;; `completions-annotations' face if no other faces are
                           ;; present. We use a custom `corfu-annotations' face to
                           ;; allow further styling which fits better for popups.
                           (unless (text-property-not-all 0 (length suff) 'face nil suff)
                             (setq suff (propertize suff 'face 'corfu-annotations)))
                           (list cand "" suff)))
              (cl-loop for cand in cands collect (list cand "" "")))))
    (cl-loop for x in cands for (c . _) = x do
             (when mf
               (setf (cadr x) (funcall mf c)))
             (when (and dep (funcall dep c))
               (setcar x (setq c (substring c)))
               (add-face-text-property 0 (length c) 'corfu-deprecated 'append c)))
    (cons mf cands)))

(cl-defgeneric corfu--prepare ()
  "Insert selected candidate unless command is marked to continue completion."
  (corfu--preview-delete)
  ;; Ensure that state is initialized before next Corfu command
  (when (and (symbolp this-command) (string-prefix-p "corfu-" (symbol-name this-command)))
    (corfu--update))
  ;; If the next command is not listed in `corfu-continue-commands', insert the
  ;; currently selected candidate and bail out of completion. This way you can
  ;; continue typing after selecting a candidate. The candidate will be inserted
  ;; and your new input will be appended.
  (and (corfu--preview-current-p) (eq corfu-preview-current 'insert)
       ;; See the comment about `overriding-local-map' in `corfu--post-command'.
       (not (or overriding-terminal-local-map
                (corfu--match-symbol-p corfu-continue-commands this-command)))
       (corfu--insert 'exact)))

(cl-defgeneric corfu--exhibit (&optional auto)
  "Exhibit Corfu UI.
AUTO is non-nil when initializing auto completion."
  (pcase-let ((`(,beg ,end ,table ,pred . ,_) completion-in-region--data)
              (`(,str . ,pt) (corfu--update 'interruptible)))
    (cond
     ;; 1) Single exactly matching candidate and no further completion is possible.
     ((and (not (equal str ""))
           (equal (car corfu--candidates) str) (not (cdr corfu--candidates))
           (not (eq corfu-on-exact-match 'show))
           (or auto corfu-on-exact-match)
           (not (consp (completion-try-completion str table pred pt corfu--metadata))))
      ;; Quit directly when initializing auto completion.
      (if (or auto (eq corfu-on-exact-match 'quit))
          (corfu-quit)
        (corfu--done (car corfu--candidates) 'finished nil)))
     ;; 2) There exist candidates => Show candidates popup.
     (corfu--candidates
      (let ((pos (posn-at-point (+ beg (length corfu--base)))))
        (corfu--preview-current beg end)
        (corfu--candidates-popup pos)))
     ;; 3) No candidates & `corfu-quit-no-match' & initialized => Confirmation popup.
     ((pcase-exhaustive corfu-quit-no-match
        ('t nil)
        ('nil corfu--input)
        ('separator (seq-contains-p (car corfu--input) corfu-separator)))
      (corfu--popup-show (posn-at-point beg) 0 8 '(#("No match" 0 8 (face italic)))))
     ;; 4) No candidates & auto completing or initialized => Quit.
     ((or auto corfu--input) (corfu-quit)))))

(cl-defgeneric corfu--teardown (buffer)
  "Tear-down Corfu in BUFFER, which might be dead at this point."
  (corfu--popup-hide)
  (corfu--preview-delete)
  (remove-hook 'post-command-hook #'corfu--post-command)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (remove-hook 'window-selection-change-functions #'corfu--window-change 'local)
      (remove-hook 'window-buffer-change-functions #'corfu--window-change 'local)
      (remove-hook 'pre-command-hook #'corfu--prepare 'local)
      (accept-change-group corfu--change-group)))
  (cl-loop for (k . v) in corfu--initial-state do (set k v)))

(defun corfu-sort-length-alpha (list)
  "Sort LIST by length and alphabetically."
  (sort list #'corfu--length-string<))

(defun corfu-quit ()
  "Quit Corfu completion."
  (interactive)
  (completion-in-region-mode -1))

(defun corfu-reset ()
  "Reset Corfu completion.
This command can be executed multiple times by hammering the ESC key.  If a
candidate is selected, unselect the candidate.  Otherwise reset the input.  If
there hasn't been any input, then quit."
  (interactive)
  (if (/= corfu--index corfu--preselect)
      (progn
        (corfu--goto -1)
        (setq this-command #'corfu-first))
    ;; Cancel all changes and start new change group.
    (pcase-let* ((`(,beg ,end . ,_) completion-in-region--data)
                 (str (buffer-substring-no-properties beg end)))
      (cancel-change-group corfu--change-group)
      (activate-change-group (setq corfu--change-group (prepare-change-group)))
      ;; Quit when resetting, when input did not change.
      (when (equal str (buffer-substring-no-properties beg end))
        (corfu-quit)))))

(defun corfu-insert-separator ()
  "Insert a separator character, inhibiting quit on completion boundary.
If the currently selected candidate is previewed, jump to the input
prompt instead.  See `corfu-separator' for more details."
  (interactive)
  (if (not (corfu--preview-current-p))
      (insert corfu-separator)
    (corfu--goto -1)
    (unless (or (= (car completion-in-region--data) (point))
                (= (char-before) corfu-separator))
      (insert corfu-separator))))

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
  "Go to first candidate.
If the first candidate is already selected, go to the prompt."
  (interactive)
  (corfu--goto (if (> corfu--index 0) 0 -1)))

(defun corfu-last ()
  "Go to last candidate."
  (interactive)
  (corfu--goto (1- corfu--total)))

(defun corfu-prompt-beginning (arg)
  "Move to beginning of the prompt line.
If the point is already the beginning of the prompt move to the
beginning of the line.  If ARG is not 1 or nil, move backward ARG - 1
lines first."
  (interactive "^p")
  (let ((beg (car completion-in-region--data)))
    (if (or (not (eq arg 1))
            (and (= corfu--preselect corfu--index) (= (point) beg)))
        (move-beginning-of-line arg)
      (corfu--goto -1)
      (goto-char beg))))

(defun corfu-prompt-end (arg)
  "Move to end of the prompt line.
If the point is already the end of the prompt move to the end of
the line.  If ARG is not 1 or nil, move forward ARG - 1 lines
first."
  (interactive "^p")
  (let ((end (cadr completion-in-region--data)))
    (if (or (not (eq arg 1))
            (and (= corfu--preselect corfu--index) (= (point) end)))
        (move-end-of-line arg)
      (corfu--goto -1)
      (goto-char end))))

(defun corfu-complete ()
  "Complete current input.
If a candidate is selected, insert it.  Otherwise invoke
`corfu-expand'.  Return non-nil if the input has been expanded."
  (interactive)
  (if (< corfu--index 0)
      (corfu-expand)
    ;; Continue completion with selected candidate.  Exit with status 'finished
    ;; if input is a valid match and no further completion is
    ;; possible. Additionally treat completion as finished if at the end of a
    ;; boundary, even if other longer candidates would still match, since the
    ;; user invoked `corfu-complete' with an explicitly selected candidate!
    (pcase-let ((`(,_beg ,_end ,table ,pred . ,_) completion-in-region--data)
                (newstr (corfu--insert nil)))
      (and (test-completion newstr table pred)
           (or (not (consp (completion-try-completion
                            newstr table pred (length newstr)
                            (completion-metadata newstr table pred))))
               (equal (completion-boundaries newstr table pred "") '(0 . 0)))
           (corfu--done newstr 'finished nil))
      t)))

(defun corfu-expand ()
  "Expands the common prefix of all candidates.
If the currently selected candidate is previewed, invoke
`corfu-complete' instead.  Expansion relies on the completion
styles via `completion-try-completion'.  Return non-nil if the
input has been expanded."
  (interactive)
  (if (corfu--preview-current-p)
      (corfu-complete)
    (pcase-let* ((`(,beg ,end ,table ,pred . ,_) completion-in-region--data)
                 (pt (max 0 (- (point) beg)))
                 (str (buffer-substring-no-properties beg end)))
      (pcase (completion-try-completion str table pred pt corfu--metadata)
        ('t
         (goto-char end)
         (corfu--done str 'finished corfu--candidates)
         t)
        ((and `(,newstr . ,newpt) (guard (not (and (= pt newpt) (equal newstr str)))))
         (corfu--replace beg end newstr)
         (goto-char (+ beg newpt))
         ;; Exit with status 'finished if input is a valid match
         ;; and no further completion is possible.
         (and (test-completion newstr table pred)
              (not (consp (completion-try-completion
                           newstr table pred newpt
                           (completion-metadata (substring newstr 0 newpt) table pred))))
              (corfu--done newstr 'finished corfu--candidates))
         t)))))

(defun corfu-insert ()
  "Insert current candidate.
Quit if no candidate is selected."
  (interactive)
  (if (>= corfu--index 0)
      (corfu--insert 'finished)
    (corfu-quit)))

(defun corfu-send ()
  "Insert current candidate and send it when inside comint or eshell."
  (interactive)
  (corfu-insert)
  (cond
   ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
    (eshell-send-input))
   ((and (derived-mode-p 'comint-mode) (fboundp 'comint-send-input))
    (comint-send-input))))

;;;###autoload
(define-minor-mode corfu-mode
  "COmpletion in Region FUnction."
  :group 'corfu :keymap corfu-mode-map
  (cond
   (corfu-mode
    (and corfu-auto (add-hook 'post-command-hook #'corfu--auto-post-command nil 'local))
    (setq-local completion-in-region-function #'corfu--in-region))
   (t
    (remove-hook 'post-command-hook #'corfu--auto-post-command 'local)
    (kill-local-variable 'completion-in-region-function))))

(defcustom global-corfu-modes t
  "List of modes where Corfu should be enabled by `global-corfu-mode'.
The variable can either be t, nil or a list of t, nil, mode
symbols or elements of the form (not modes).  Examples:
  - Enable everywhere, except in Org: ((not org-mode) t).
  - Enable in programming modes except Python: ((not python-mode) prog-mode).
  - Enable only in text modes: (text-mode)."
  :type '(choice (const t) (repeat sexp))
  :group 'corfu)

;; TODO use `:predicate' on Emacs 29
(defcustom global-corfu-minibuffer t
  "Corfu should be enabled in the minibuffer by `global-corfu-mode'.
The variable can either be t, nil or a custom predicate function.  If
the variable is set to t, Corfu is only enabled if the minibuffer has
local `completion-at-point-functions'."
  :type '(choice (const t) (const nil) function)
  :group 'corfu)

;;;###autoload
(define-globalized-minor-mode global-corfu-mode
  corfu-mode corfu--on
  :group 'corfu
  (remove-hook 'minibuffer-setup-hook #'corfu--minibuffer-on)
  (when (and global-corfu-mode global-corfu-minibuffer)
    (add-hook 'minibuffer-setup-hook #'corfu--minibuffer-on 100)))

(defun corfu--on ()
  "Enable `corfu-mode' in the current buffer respecting `global-corfu-modes'."
  (when (and (not noninteractive) (not (eq (aref (buffer-name) 0) ?\s))
             ;; TODO use `:predicate' on Emacs 29
             (or (eq t global-corfu-modes)
                 (eq t (cl-loop for p in global-corfu-modes thereis
                                (pcase-exhaustive p
                                  ('t t)
                                  ('nil 0)
                                  ((pred symbolp) (and (derived-mode-p p) t))
                                  (`(not . ,m) (and (seq-some #'derived-mode-p m) 0)))))))
    (corfu-mode 1)))

(defun corfu--minibuffer-on ()
  "Enable `corfu-mode' in the minibuffer respecting `global-corfu-minibuffer'."
  (when (and global-corfu-minibuffer (not noninteractive)
             (if (functionp global-corfu-minibuffer)
                 (funcall global-corfu-minibuffer)
               (local-variable-p 'completion-at-point-functions)))
    (corfu-mode 1)))

;; Do not show Corfu commands with M-X
(dolist (sym '( corfu-next corfu-previous corfu-first corfu-last corfu-quit corfu-reset
                corfu-complete corfu-insert corfu-scroll-up corfu-scroll-down corfu-expand
                corfu-send corfu-insert-separator corfu-prompt-beginning corfu-prompt-end
                corfu-info-location corfu-info-documentation ;; autoloads in corfu-info.el
                corfu-quick-jump corfu-quick-insert corfu-quick-complete)) ;; autoloads in corfu-quick.el
  (put sym 'completion-predicate #'ignore))

(defun corfu--capf-wrapper-advice (orig fun which)
  "Around advice for `completion--capf-wrapper'.
The ORIG function takes the FUN and WHICH arguments."
  (if corfu-mode (corfu--capf-wrapper fun t) (funcall orig fun which)))

(defun corfu--eldoc-advice ()
  "Return non-nil if Corfu is currently not active."
  (not (and corfu-mode completion-in-region-mode)))

;; Install advice which fixes `completion--capf-wrapper', such that it respects
;; the completion styles for non-exclusive Capfs. See also the fixme comment in
;; the `completion--capf-wrapper' function in minibuffer.el.
(advice-add #'completion--capf-wrapper :around #'corfu--capf-wrapper-advice)

;; Register Corfu with ElDoc
(advice-add #'eldoc-display-message-no-interference-p
            :before-while #'corfu--eldoc-advice)
(eldoc-add-command #'corfu-complete #'corfu-insert #'corfu-expand #'corfu-send)

(provide 'corfu)
;;; corfu.el ends here
