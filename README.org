#+title: corfu.el - Completion Overlay Region FUnction
#+author: Daniel Mendler
#+language: en
#+export_file_name: corfu.texi
#+texinfo_dir_category: Emacs
#+texinfo_dir_title: Corfu: (corfu).
#+texinfo_dir_desc: Completion Overlay Region FUnction

#+html: <a href="http://elpa.gnu.org/packages/corfu.html"><img alt="GNU ELPA" src="https://elpa.gnu.org/packages/corfu.svg"/></a>
#+html: <a href="http://elpa.gnu.org/devel/corfu.html"><img alt="GNU-devel ELPA" src="https://elpa.gnu.org/devel/corfu.svg"/></a>

* Introduction

Corfu enhances the default completion in region function with a completion
overlay. The current candidates are shown in a popup below or above the point.
Corfu can be considered the minimalistic ~completion-in-region~ counterpart of
the [[https://github.com/minad/vertico][Vertico]] minibuffer UI.

Corfu is a minimal package (~600 lines of code without whitespace and comments).
In contrast to the featureful and complex Company package, Corfu concentrates on
the completion UI and does not include custom completion backends. Completions
are either provided by commands like ~dabbrev-completion~ or by pluggable
backends (~completion-at-point-functions~, Capfs). Many programming language
major modes implement a Capf. Furthermore the language server packages, [[https://github.com/joaotavora/eglot][Eglot]]
and [[https://github.com/emacs-lsp/lsp-mode][Lsp-mode]], both use Capfs which talk to the LSP server to retrieve the
completions.

The Emacs builtin Icomplete is technically comparable to Corfu and Vertico.
Icomplete implements both ~completion-in-region~ and minibuffer completion in a
single package. Corfu and Vertico are two separate packages in order to optimize
the UI for the two distinct use cases, also leading to code bases which are
easier to understand.

*NOTE*: Corfu uses child frames to show the popup; on non-graphical displays it
will fall back to the default setting of the ~completion-in-region-function~.

[[https://github.com/minad/corfu/blob/main/screenshot.png?raw=true]]

* Features

- Popup display with scrollbar indicator and arrow key navigation
- The popup must be summoned explicitly by pressing =TAB=
- The current candidate is inserted with =TAB= and selected with =RET=
- Candidates sorting by prefix, string length and alphabetically
- Completion is automatically terminated after candidate selection
- Filter string can contain arbitrary characters and spaces (needed
  when filtering with the [[https://github.com/oantolin/orderless][Orderless]] completion style)
- Deferred completion style highlighting for performance
- Jumping to location/documentation of current candidate (Company extension)
- Support for ~annotation-function~ and ~affixation-function~

Notable non-feature: Timer-based idle completions are not supported.

* Configuration

Corfu is available from [[http://elpa.gnu.org/packages/corfu.html][GNU ELPA]], such that it can be installed directly via
~package-install~. After installation, the local minor mode can be enabled with
=M-x corfu-mode=. In order to configure Corfu and other packages in your
init.el, you may want to use ~use-package~. I recommend to give Orderless
completion a try, which is different from the familiar prefix TAB completion.
However Corfu works well with the default completion styles, the use of
Orderless is not a necessity. Here is an example configuration:

#+begin_src emacs-lisp
  ;; Configure corfu
  (use-package corfu
    ;; Optionally use TAB for cycling, default is `corfu-complete'.
    ;; :bind (:map corfu-map
    ;;        ("TAB" . corfu-next)
    ;;        ("S-TAB" . corfu-previous))

    ;; You may want to enable Corfu only for certain modes.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))


    ;; Recommended: Enable Corfu globally.
    ;; This is recommended since dabbrev can be used globally (M-/).
    :init
    (corfu-global-mode)

    :config

    ;; Optionally enable cycling for `corfu-next' and `corfu-previous'.
    ;; (setq corfu-cycle t)
  )

  ;; Optionally use the `orderless' completion style.
  ;; Enable `partial-completion' for files to allow path expansion.
  ;; You may prefer to use `initials' instead of `partial-completion'.
  (use-package orderless
    :init
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion))))))

  ;; Dabbrev works with Corfu
  (use-package dabbrev
    ;; Swap M-/ and C-M-/
    :bind (("M-/" . dabbrev-completion)
           ("C-M-/" . dabbrev-expand)))

  ;; A few more useful configurations...
  (use-package emacs
    :init
    ;; TAB cycle if there are only few candidates
    (setq completion-cycle-threshold 3)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (setq tab-always-indent 'complete))
#+end_src

* Key bindings

Corfu uses a transient keymap ~corfu-map~ which is active while the popup is shown.
The keymap defines the following remappings and bindings:

- ~beginning-of-buffer~ -> ~corfu-first~
- ~end-of-buffer~ -> ~corfu-last~
- ~scroll-down-command~ -> ~corfu-scroll-down~
- ~scroll-up-command~ -> ~corfu-scroll-up~
- ~next-line~, =down=, =M-n= -> ~corfu-next~
- ~previous-line~, =up=, =M-p= -> ~corfu-previous~
- ~completion-at-point~, =TAB= -> ~corfu-complete~
- =RET= -> ~corfu-insert~
- =M-g= -> ~corfu-show-location~
- =M-h= -> ~corfu-show-documentation~
- =C-g=, =ESC ESC ESC= -> ~corfu-abort~

* Complementary packages

Corfu works well together with all packages providing code completion via the
~completion-at-point-functions~. Furthermore it supports various completion
styles, including the advanced [[https://github.com/oantolin/orderless][Orderless]] completion style, where the filtering
expressions are separated by spaces.

You may also want to look into my [[https://github.com/minad/vertico][Vertico]] package. Vertico is the minibuffer
counterpart of Corfu.

* Caveats

Corfu works in most scenarios. However there are a few known technical caveats.

- No support for idle completions.
- Corfu falls back to the default ~completion-in-region-function~ on
  non-graphical displays, since is displayed using child frames.
- The abort handling could be improved, for example the input could be undone.
- The ~completion-in-region-mode-predicate~ is ignored in order to
  give the completion style full control. The predicate asks the backend if
  the starting point of the completion has changed.
- Company kind icons, docsig and match data are not supported
  (~company-kind~, ~company-docsig~, ~company-match~).
- No support for multi-backends like Company (Implement a multi-capf?).
- No sorting by history, since ~completion-at-point~ does not
  maintain a history (See branch =history= for a possible solution).

* Contributions

Since this package is part of [[http://elpa.gnu.org/packages/corfu.html][GNU ELPA]] contributions require a copyright
assignment to the FSF.
