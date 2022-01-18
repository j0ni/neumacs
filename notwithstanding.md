
# Table of Contents

1.  [Notwithstanding](#orgcf5bdc2)
    1.  [Pre-amble](#orgc283eec)
    2.  [Some building blocks](#org3c4e4bd)
        1.  [Useful functions](#org27373cc)
        2.  [Undo a gotcha](#orge55959c)
        3.  [A little about the package manager](#orgef22e7a)
        4.  [Global Keymap Hacks](#org90d0b71)
        5.  [Global Switches](#org5c29699)
    3.  [Install packages](#org6528bc7)
    4.  [Baseline Emacs Configuration](#org581e40e)
    5.  [Completion](#org788e575)
        1.  [Minibuffer setup](#orgdb7250b)
        2.  [Builtin completion configuration](#org42546b4)
        3.  [Extra builtins](#org61d5d39)
        4.  [yas-snippets](#org3740ed3)
        5.  [Vertico](#orgff849f2)
        6.  [Marginalia](#org79279a5)
    6.  [Package Configuration](#org691a681)
        1.  [ibuffer](#org57fdc3f)
        2.  [Key chords](#org7e838f9)
        3.  [Flymake](#orgeaeffb0)
        4.  [Flycheck](#org67e6a88)
        5.  [Consult](#org651a463)
        6.  [LSP](#orgecd2bad)
        7.  [Find File in Project](#orgc4e324f)
        8.  [IRC - ERC and RCIRC](#orga0aefc3)
        9.  [Undo-fu](#org08fede6)
        10. [exec-path-from-shell](#org4417fbe)
        11. [Highlight TODO Mode](#org65d2890)
        12. [Volatile Highlights](#org0be92ef)
        13. [Themes!](#orgff044aa)
        14. [Rainbow Mode](#orgcd40cdf)
        15. [Rainbow Delimiters Mode](#org7af2676)
        16. [Diff Highlight Mode](#org563ab4f)
        17. [Git Time Machine](#org27f469e)
        18. [Expand Region](#orgb181a98)
        19. [Anzu](#orgc9c9e56)
        20. [Browse Kill Ring](#orgd36d6f8)
        21. [Magit](#orge2464d0)
        22. [Idle highlight mode](#orgd956636)
        23. [Paredit](#orga9e9753)
        24. [Scheme](#org9727a5d)
        25. [Which Key](#org911dc70)
        26. [Window Switcher](#org30fc608)
        27. [Web mode and webbish stuff](#orgd5dbd7c)
        28. [Common Lisp - Sly](#orgdfe11d9)
        29. [Clojure](#org9dc2752)
        30. [Lua and Fennel](#org6d413d0)
        31. [Ruby](#org128ae96)
        32. [C/C++](#org9a67f01)
        33. [Markdown](#org287bec0)
        34. [Purescript](#orgf73efcc)
        35. [Typescript](#org1207ece)
        36. [Evaluation overlays](#org8f04766)
        37. [Rust](#org438a553)
        38. [Org Mode](#orgc41f1fe)
        39. [ELFeed - RSS Reader](#org070b716)
        40. [Telega](#orgf3ad7fe)
        41. [Minions](#org1ed1c36)
        42. [Icons](#org585b542)
        43. [Haskell](#orge23d95a)
        44. [Olivetti Mode](#orge57d0a7)
        45. [Move Text](#orgd40b02f)
        46. [Set all the fonts one last time](#org9071105)
        47. [Mu 4 Emacs](#orge0f3941)
        48. [Crypto setup](#org3b6a0c5)
        49. [Custom file configuration](#orgd9d6e1f)


<a id="orgcf5bdc2"></a>

# Notwithstanding

my intent to write a literate configuration for my emacs, in all likelihood this will end up being a half-baked solution to a non-problem I don't have. Anyway, let's get to it. I believe there should be a pre-amble.


<a id="orgc283eec"></a>

## Pre-amble

    ;;; init.el --- emacs config -*- no-byte-compile: t ; lexical-binding: t; -*-
    ;;;
    ;;; Commentary:
    ;;;
    ;;; Hi.
    ;;;
    ;;; Code:


<a id="org3c4e4bd"></a>

## Some building blocks


<a id="org27373cc"></a>

### Useful functions

    ;; handy fns
    (defun j0ni/disable-truncate-lines ()
      (interactive)
      (setq-local truncate-lines nil))
    (defun j0ni/enable-truncate-lines ()
      (interactive)
      (setq-local truncate-lines t))
    (defun j0ni/disable-word-wrap ()
      (interactive)
      (setq-local word-wrap nil))
    (defun j0ni/enable-word-wrap ()
      (interactive)
      (setq-local word-wrap t))

    ;; Shamelessly lifted from @zarkone's config, and refactored
    (defun j0ni/delete-whitespace (&optional backward-only)
      "Replaces all spaces, tabs and newlinesaround point with a single space.
    If BACKWARD-ONLY is non-nil, only delete them before point."
      (interactive "*P")
      (unless backward-only
        (j0ni/backward-delete-whitespace))
      (j0ni/forward-delete-whitespace)
      (unless backward-only (insert " ")))

    (defun j0ni/forward-delete-whitespace ()
      (interactive)
      (let ((orig-pos (point)))
        (delete-region
         (progn
           (skip-chars-forward " \t\n")
           (constrain-to-field nil orig-pos t))
         orig-pos)))

    (defun j0ni/backward-delete-whitespace ()
      (interactive)
      (let ((orig-pos (point)))
        (delete-region
         orig-pos
         (progn
           (skip-chars-backward " \t\n")
           (constrain-to-field nil orig-pos)))))

    ;; Been missing yooooou
    (defun j0ni/insert-shrug ()
      (interactive)
      (insert "¯\\_(ツ)_/¯"))

    (defun j0ni/reload-config ()
      (interactive)
      (load-file (expand-file-name "early-init.el" user-emacs-directory))
      (load-file (expand-file-name "init.el" user-emacs-directory))
      (j0ni/init-frame))

    ;; Don't know where I found this, but it I didn't write it
    (defun j0ni/toggle-window-split ()
      "Vertical split shows more of each line, horizontal split shows
    more lines. This code toggles between them. It only works for
    frames with exactly two windows."
      (interactive)
      (if (= (count-windows) 2)
          (let* ((this-win-buffer (window-buffer))
                 (next-win-buffer (window-buffer (next-window)))
                 (this-win-edges (window-edges (selected-window)))
                 (next-win-edges (window-edges (next-window)))
                 (this-win-2nd (not (and (<= (car this-win-edges)
                                             (car next-win-edges))
                                         (<= (cadr this-win-edges)
                                             (cadr next-win-edges)))))
                 (splitter
                  (if (= (car this-win-edges)
                         (car (window-edges (next-window))))
                      'split-window-horizontally
                    'split-window-vertically)))
            (delete-other-windows)
            (let ((first-win (selected-window)))
              (funcall splitter)
              (if this-win-2nd (other-window 1))
              (set-window-buffer (selected-window) this-win-buffer)
              (set-window-buffer (next-window) next-win-buffer)
              (select-window first-win)
              (if this-win-2nd (other-window 1))))))

    (defun zarkone/vertical-three-windows-layout ()
      "Vertical, three window layout"
      (interactive)
      (delete-other-windows)
      (split-window-horizontally)
      (split-window-horizontally)
      (balance-windows))

    (defun j0ni/read-string-from-file (file-path)
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string)))


<a id="orge55959c"></a>

### Undo a gotcha

Sometimes I accidentally find myself in the C-x map, and hit C-g to get the hell out, in a panic. But it's unbound, which is effectively the same thing, but annoying. Why be unbound, when this is what I probably meant?

OTOH, how to make this future proof&#x2026; check if it's bound?

    (when (not (keymap-global-lookup "C-x C-g"))
      (message "Fixing C-x C-g")
      (keymap-global-set "C-x C-g" #'keyboard-quit))


<a id="orgef22e7a"></a>

### A little about the package manager

So we started (top of the init.el, before tangling) by installing the packaging system, which is straight.el, found at <https://github.com/raxod502/straight.el>.

Straight is pretty cool, but also has become quite complex and fiddly, with a lot of documentation not much of which is easy to follow. For now I'm good, but I might resort either to my own git submodules with some use-package load magic, or head back toward package.el.

Perhaps next time I'm as frustrated as I was trying to figure out how to override vertico's recipe. But for now&#x2026;


<a id="org90d0b71"></a>

### Global Keymap Hacks

The next thing is a set of key mappings for getting special characters, like umlauts and lambdas, which has nothing at all to do with input methods and other dark magic.

    (defvar j0ni/unicode-mapping-alist
      '((U . "Ü")
        (u . "ü")
        (A . "Ä")
        (a . "ä")
        (O . "Ö")
        (o . "ö")
        (S . "ẞ")
        (s . "ß")
        (l . "λ")))

    (defun j0ni/bind-unicode-shortcut (map ch)
      "Simplify the binding a tiny bit."
      (let ((new-ch (alist-get ch j0ni/unicode-mapping-alist)))
        (define-key map (kbd (symbol-name ch))
          `(lambda () (interactive) (insert ,new-ch)))))

    (defvar j0ni/unicode-shortcut-map)
    (setq j0ni/unicode-shortcut-map
          (let ((map (make-sparse-keymap)))
            (j0ni/bind-unicode-shortcut map 'U)
            (j0ni/bind-unicode-shortcut map 'u)
            (j0ni/bind-unicode-shortcut map 'A)
            (j0ni/bind-unicode-shortcut map 'a)
            (j0ni/bind-unicode-shortcut map 'O)
            (j0ni/bind-unicode-shortcut map 'o)
            (j0ni/bind-unicode-shortcut map 'S)
            (j0ni/bind-unicode-shortcut map 's)
            (j0ni/bind-unicode-shortcut map 'l)
            map))

    (fset 'j0ni/unicode-shortcut-map j0ni/unicode-shortcut-map)

    ;; This isn't available in Org-mode - find a better one
    (keymap-global-set "C-'" 'j0ni/unicode-shortcut-map)


<a id="org5c29699"></a>

### Global Switches

This is a useful gate for setting up bindings and other Mac OS bits and pieces.

    (defvar j0ni/is-mac (memq window-system '(mac ns))
      "This is a useful gate for setting up specific keybindings")

Honestly, there are more of these, but I moved them to early-init.el for reasons that may have become lost in the mists of time. Mostly fonts.


<a id="org6528bc7"></a>

## Install packages

This is how you merge in changes to a straight recipe. Took me a good long time to figure out how to do this correctly, it was not at all obvious.

    (straight-override-recipe '(vertico :inherit t :files (:defaults "extensions/*.el")))
    ;; (straight-override-recipe '(the-matrix-theme :inherit t :fork "j0ni/matrix-emacs-theme"))

Here we go. This installs all the packages and their dependencies (implicitly, though there's a lot of overlap). Configuration happens down the road, because sometimes broken configuration will prevent the bootstrapping of the whole installation. That's not the end of the world, but it is a pain in the ass. More notes about them inline with config.

Note that these commands do not \`require\` anything, but they do construct the autoloads where the packages declare them.

Future me may well ditch the autoloads completely in favour of git submodules, no build step, and explicit use-package configuration. That's because use-package itself will implicitly load functions when you declare them via :hooks or :binds, and explicitly via :commands. In the context of another package manager such as straight.el or package.el, that's just downright confusing, because it isn't clear what was required and what was autoloaded. In a git submodule world that might not be so confusing.

    (straight-use-package 'all-the-icons)
    (straight-use-package 'anzu)
    (straight-use-package 'browse-at-remote)
    (straight-use-package 'browse-kill-ring)
    (straight-use-package 'cider)
    (straight-use-package 'clojure-mode)
    (straight-use-package 'company)
    (straight-use-package 'consult)
    (straight-use-package 'consult-flycheck)
    (straight-use-package 'consult-lsp)
    (straight-use-package 'diff-hl)
    (straight-use-package 'dockerfile-mode)
    (straight-use-package 'elfeed)
    (straight-use-package 'erc)
    (straight-use-package 'eros)
    (straight-use-package 'exec-path-from-shell)
    (straight-use-package 'expand-region)
    (straight-use-package 'fennel-mode)
    (straight-use-package 'find-file-in-project)
    (straight-use-package 'flycheck)
    (straight-use-package 'flycheck-eldev)
    (straight-use-package 'geiser)
    (straight-use-package 'geiser-chez)
    (straight-use-package 'geiser-chicken)
    (straight-use-package 'geiser-guile)
    (straight-use-package 'ggtags)
    (straight-use-package 'git-timemachine)
    (straight-use-package 'graphql-mode)
    (straight-use-package 'haskell-mode)
    (straight-use-package 'hl-todo)
    (straight-use-package 'ibuffer-vc)
    (straight-use-package 'idle-highlight)
    (straight-use-package 'inf-clojure)
    (straight-use-package 'inf-ruby)
    (straight-use-package 'json-mode)
    (straight-use-package 'lsp-mode)
    (straight-use-package 'lsp-ui)
    (straight-use-package 'lua-mode)
    (straight-use-package 'key-chord)
    (straight-use-package 'magit)
    (straight-use-package 'marginalia)
    (straight-use-package 'markdown-mode)
    (straight-use-package 'the-matrix-theme)
    (straight-use-package 'minions)
    (straight-use-package 'modus-themes)
    (straight-use-package 'monroe)
    (straight-use-package 'move-text)
    (straight-use-package 'olivetti)
    (straight-use-package 'org-roam)
    (straight-use-package 'org-super-agenda)
    (straight-use-package 'paredit)
    (straight-use-package 'pinentry)
    (straight-use-package 'psc-ide)
    (straight-use-package 'purescript-mode)
    (straight-use-package 'racket-mode)
    (straight-use-package 'rainbow-delimiters)
    (straight-use-package 'rainbow-mode)
    (straight-use-package 'rbenv)
    (straight-use-package 'restclient)
    (straight-use-package 'ruby-mode)
    (straight-use-package 'rustic)
    (straight-use-package 'simple-httpd)
    (straight-use-package 'sly)
    (straight-use-package 'sly-asdf)
    (straight-use-package 'sly-macrostep)
    (straight-use-package 'sly-quicklisp)
    (straight-use-package 'switch-window)
    (straight-use-package 'telega)
    (straight-use-package 'typescript-mode)
    (straight-use-package 'undo-fu)
    (straight-use-package 'undo-fu-session)
    (straight-use-package 'vertico)
    (straight-use-package 'volatile-highlights)
    (straight-use-package 'web-mode)
    (straight-use-package 'which-key)
    (straight-use-package 'yaml-mode)
    (straight-use-package 'yasnippet)


<a id="org581e40e"></a>

## Baseline Emacs Configuration

This is where the config starts, and the following are all based on built-in functionality.

I dislike super long lines, but I do not care much for obsolete terminals, so 80 columns is silly.

    (setq whitespace-line-column 100)
    (setq whitespace-style '(face trailing lines-tail tabs))
    (add-hook 'prog-mode-hook #'whitespace-mode)

I don't understand why conf mode (ini, toml, etc) doesn't have matched parens, I mean, you don't ever just open a paren in them do you?

    (add-hook 'conf-mode-hook #'electric-pair-local-mode)

Modern emacs can be built with native just-in-time compilation built in. Straight will kick off AOT compilation of anything that's loaded (or at least I think it's straight), which happens asynchronously via the (native-compile-async) command. I very rarely care to watch that happen, and I **definitely** don't care to have it pop up in a split while emacs is starting up, or indeed any time I open a file with a mode that has yet to be natively compiled.

So, begin suppressive actions:

    (setq warning-suppress-types '((comp)))

These are mostly settings that emacs considers to be "customizations".

    (setq epa-pinentry-mode 'loopback)
    (setq inhibit-startup-screen t)
    (setq auto-revert-verbose t)
    (setq vc-follow-symlinks t)
    (setq find-file-suppress-same-file-warnings t)
    (setq comint-prompt-read-only t)
    (setq select-enable-clipboard t)
    (setq select-enable-primary t)
    (setq uniquify-buffer-name-style 'forward)
    (setq save-interprogram-paste-before-kill t)
    (setq compilation-always-kill t)
    (setq compilation-ask-about-save nil)
    (setq apropos-do-all t)
    (setq mouse-yank-at-point t)
    (setq save-place-file (concat user-emacs-directory ".places"))
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory ".backups"))))
    (setq enable-local-variables t) ;; :all
    (setq confirm-kill-emacs nil)
    (setq sentence-end-double-space nil)
    (setq delete-old-versions t)
    (setq version-control t)
    (setq custom-safe-themes t)
    (setq mouse-wheel-progressive-speed t)              ; accelerate scrolling
    (setq shr-color-visible-luminance-min 90)
    (advice-add #'shr-colorize-region
                :around (defun shr-no-colorise-region (&rest ignore)))

    ;; gotta find the berlin coords
    ;; 43.67066, -79.30211 - location
    ;; (setq calendar-longitude 43.67066)
    ;; (setq calendar-latitude -79.30211)
    ;; (setq calendar-location-name "Toronto")

Configure keyboard for MacOS. This repurposes:

; - alt             -> meta
; - right alt       -> same as left (meta)
; - left command    -> meta
; - right command   -> super
; - function key    -> ignore

    (when j0ni/is-mac
      (setq ns-alternate-modifier 'meta)
      (setq ns-right-alternate-modifier 'left)
      (setq ns-command-modifier 'meta)
      (setq ns-right-command-modifier 'super)
      (setq ns-function-modifier 'none))

Pick a browser based on OS. I recently added the \`gnu/linux\` clause to try to make more use of eww. It isn't great, but it can be tamed (see shr-color setting above). My only fear is that I'll waste a use-once URL by accident due to some missing functionality. Meh.

    (setq-default browse-url-browser-function
                  (cl-case system-type
                    ((darwin macos) 'browse-url-default-macosx-browser)
                    ((gnu/linux) 'eww-browse-url)
                    (t 'browse-url-default-browser)))

Maybe if I didn't do this, I'd make fewer rash decisions.

    (defalias 'yes-or-no-p 'y-or-n-p)

I mean, we do live in this world now.

    (set-language-environment "UTF-8")
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-buffer-file-coding-system 'utf-8)
    (set-file-name-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)

Pixel scroll settings are amazing, and even though I've been using emacs build from mainline for ages I totally didn't know about it. Because that's the kind of bleeding edge life I lead. One of pointless risks, taken without regard to any potential benefits or even stopping to see what they might be.

    (pixel-scroll-precision-mode 1)

Be less of a jerk (sorry everyone around me is now speaking and seeing double entendres, I can't help it).

    (setq scroll-step 0)
    (setq scroll-margin 2)
    (setq auto-window-vscroll nil)
    ;; be sure to set this to 0 in any auto-scrolling buffers
    (setq scroll-conservatively 100000)
    (setq scroll-preserve-screen-position t)
    (setq next-screen-context-lines 3)

Some emacs droppings are more annoying than they are useful. And some things I'm not sure I understand&#x2026;?

    (setq create-lockfiles nil)
    (setq redisplay-dont-pause t)
    (setq disabled-command-function nil)

Ah the alert bell, how irritating are you? But this is a nice alternative, taken directly from the emacs wiki.

    (defun flash-mode-line ()
      (invert-face 'mode-line)
      (run-with-timer 0.1 nil #'invert-face 'mode-line))

    (setq visible-bell nil)
    (setq ring-bell-function 'flash-mode-line)

Tabs. Tab should not insert tabs. Tab should indent, and ideally only to the correct location. Fuck Haskell.

Tabs should not be 8 characters wide, but they are, and if you don't let them be, many things will become horrible. Go is horrible, so there is no contradiction there.

We should absolutely not use tabs for indentation though, so make sure we never do.

    (setq-default indent-tabs-mode nil)
    (setq-default tab-width 8)
    (setq indent-tabs-mode nil)
    (setq tab-always-indent 'complete)
    (setq require-final-newline t)

This, like pixel scrolling, is something I didn't know I was missing.

    (delete-selection-mode 1)

More cosmetic tweaks, more agreeable defaults, and some things I don't understand.

    (setq load-prefer-newer t)
    (setq highlight-nonselected-windows nil)
    (setq kill-buffer-query-functions nil)
    (setq-default cache-long-scans t)
    (setq-default word-wrap nil)
    (setq-default indicate-buffer-boundaries 'left)
    (setq-default fill-column 80)
    (setq-default line-spacing 0)
    (setq-default truncate-lines t)
    (setq resize-mini-windows t)
    (setq completion-show-help nil)

Because I honestly don't care about anyone else. That's what ?w=1 is for.

    (add-hook 'before-save-hook #'delete-trailing-whitespace)

Start a few global essentials.

    (dolist (mode '(electric-indent-mode
                    show-paren-mode
                    save-place-mode
                    size-indication-mode
                    global-hl-line-mode
                    column-number-mode
                    winner-mode
                    global-auto-revert-mode))
      (funcall mode 1))

Kill a couple of less essential globals.

    (blink-cursor-mode -1)
    (remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)

Because sometimes I want to live without consult:

    (recentf-mode 1)
    (keymap-global-set "C-x M-f" #'recentf-open-files)

Dired. I am not really sure that I get it.

    (put 'dired-find-alternate-file 'disabled nil)

    ;; always delete and copy recursively
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always)

    ;; if there is a dired buffer displayed in the next window, use its
    ;; current subdir, instead of the current subdir of this dired buffer
    (setq dired-dwim-target t)

    ;; enable some really cool extensions like C-x C-j (dired-jump)
    (require 'dired-x)

Proced, which I recently discovered in bbatsov's dotfiles. It's a nice enough process table and editor.

    (keymap-global-set "C-x P" #'proced)

Some bindings I've come to depend on. I'm genuinely trying to scale down these kinds of customisations where I have probably been stomping on binds I have never ever experienced before.

    (keymap-set lisp-mode-shared-map "C-c C-k" #'eval-buffer)

    (dolist (binding
             '(("C-x C-r" . revert-buffer)
               ("C-x |" . j0ni/toggle-window-split)
               ("C-c ." . j0ni/delete-whitespace)
               ("C-c s" . j0ni/insert-shrug)
               ("C-=" . text-scale-increase)
               ("C--" . text-scale-decrease)))
      (keymap-global-set (car binding) (cdr binding)))

Command history for the minibuffer. Invaluable intell.

    (setq savehist-save-minibuffer-history t)
    (setq history-length 10000)
    (setq history-delete-duplicates t)

    (savehist-mode 1)

Time and date, and battery, for the modeline.

    (setq display-time-format "%Y-%m-%d %H:%M")
    (setq display-time-24hr-format t)
    (setq display-time-day-and-date nil)
    (setq display-time-interval 15)
    (setq display-time-default-load-average nil)
    (setq zoneinfo-style-world-list
          '(("America/Los_Angeles" "San Francisco")
            ("America/New_York" "Toronto")
            ("Europe/London" "London")
            ("Europe/Berlin" "Berlin")
            ("Asia/Hong_Kong" "Hong Kong")
            ("Asia/Tokyo" "Tokyo")))

    (display-time-mode 1)
    (display-battery-mode 1)

A little configuration for xref, which is honesly mostly totally fine.

    (setq xref-marker-ring-length 64)
    (setq xref-show-xrefs-function 'xref--show-xref-buffer) ; default
    (setq xref-show-definitions-function 'xref-show-definitions-completing-read)

Thats the end of the baseline emacs configuration.


<a id="org788e575"></a>

## Completion

This gets a special section for having so much to configure.


<a id="orgdb7250b"></a>

### Minibuffer setup

    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    (setq minibuffer-completion-confirm 'confirm)
    ;; [ ... ] instead of (default ...
    (setq minibuffer-eldef-shorten-default t)
    ;; I think this is bad for my impulsive fingers
    (setq enable-recursive-minibuffers t)
    ;; at least show us where we are
    (minibuffer-depth-indicate-mode t)
    ;; it shouldn't be disallowed
    (setq minibuffer-scroll-window t)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))

    (minibuffer-electric-default-mode 1)
    (file-name-shadow-mode 1)


<a id="org42546b4"></a>

### Builtin completion configuration

Not all of this is respected by various different systems I try out and switch between. Worth keeping it all though, so it's there when I inevitably switch to the thing that has it wrong.

    (setq completion-ignore-case t)
    (setq read-file-name-completion-ignore-case t)
    (setq read-buffer-completion-ignore-case t)
    (setq completion-cycle-threshold 3)
    (setq completions-detailed t)
    (setq completions-format 'one-column)

These define the completion algorithms used in general, and in each separate context. The list of overrides is non-exhaustive, and I cannot find any way of figuring out what all the keys should be.

Note that the way this works is, the first of these to return anything is used. So as you narrow, it may fall through the list. For this reason, there's no point in (for example) putting flex at the front, because it will always return a superset of substring. You get the drift. Confusing but a fair bit of control.

Of course, fido-mode completely ignores these settings.

    (setq completion-styles '(basic substring initials partial-completion flex))

    (setq completion-category-overrides
          '((buffer (styles . (basic substring partial-completion)))
            (file (styles . (initials partial-completion flex)))
            (unicode-name (styles . (basic substring)))
            (project-file (styles . (substring partial-completion)))
            (xref-location (styles . (substring)))
            (info-menu (styles . (basic substring)))
            (symbol-help (styles . (basic shorthand substring)))))


<a id="org61d5d39"></a>

### Extra builtins

1.  Hippie Expand

        (require 'hippie-exp)
        (setq hippie-expand-try-functions-list
              '(try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-file-name-partially
                try-complete-file-name
                try-expand-all-abbrevs
                try-expand-list
                try-expand-line
                try-complete-lisp-symbol-partially
                try-complete-lisp-symbol))

        ;; Swap M-/ and C-M-/
        (keymap-global-set "M-/" #'hippie-expand)
        (keymap-global-set "s-/" #'hippie-expand)
        (keymap-global-set "C-M-/" #'hippie-expand)

2.  Abbrev

        (require 'abbrev)
        (setq save-abbrevs 'silently)
        (setq-default abbrev-mode t)


<a id="org3740ed3"></a>

### yas-snippets

    (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))


<a id="orgff849f2"></a>

### Vertico

A fast vertical minibuffer manager which mostly plays nice with builtin stuff. Moreso than many - all but MCT, I dare say.

    ;; (setq straight-recipe-overrides nil)
    (vertico-mode 1)
    ;; this
    ;; (vertico-unobtrusive-mode 1)
    ;; or this
    (vertico-buffer-mode 1)
    ;; but not both

    (keymap-set vertico-map "RET" #'vertico-directory-enter)
    (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
    (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

This is a bit previous - I should generalize it and move it up into the consult configuration. But the principle is one I'd like to get used to. Out-of-buffer completion, with the regular completion system, whatever that may be. So we do this:

    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))

instead of this:

    ;; (require 'company)
    ;; (global-company-mode 1)


<a id="org79279a5"></a>

### Marginalia

Marginalia adds a bunch of metadata annotations to completions, which are portable across builtin completion functionality as well as things like vertico. Mostly handy info, occasionally just line filler.

    (marginalia-mode 1)


<a id="org691a681"></a>

## Package Configuration


<a id="org57fdc3f"></a>

### ibuffer

OK I lied a bit. ibuffer is built-in, but ibuffer-vc is not, and I wanted to keep this all together.

    ;; ibuffer looks much nicer than the default view
    (require 'ibuffer)

    (setq ibuffer-expert t)
    (setq ibuffer-display-summary nil)
    (setq ibuffer-use-other-window nil)
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-movement-cycle nil)
    (setq ibuffer-default-sorting-mode 'filename/process)
    (setq ibuffer-use-header-line t)
    (setq ibuffer-default-shrink-to-minimum-size nil)
    ;; (setq ibuffer-saved-filter-groups nil)
    (setq ibuffer-old-time 72)

    (keymap-global-set "C-x C-b" #'ibuffer)

    (require 'vc)
    (require 'ibuffer-vc)

    (setq ibuffer-formats
          '((mark modified read-only vc-status-mini
                  " " (name 18 18 :left :elide)
                  " " (size 9 -1 :right)
                  " " (mode 16 16 :left :elide)
                  " " (vc-status 16 16 :left)
                  " " filename-and-process)
            (mark modified read-only vc-status-mini
                  " " (name 18 18 :left :elide)
                  " " (size 9 -1 :right)
                  " " (mode 16 16 :left :elide)
                  " " (vc-status 16 16 :left)
                  " " vc-relative-file)))


    (defun j0ni/ibuffer-vc-hook ()
      (ibuffer-auto-mode 1)
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'recency)
        (ibuffer-do-sort-by-recency)))

    ;; (remove-hook 'ibuffer-hook #'j0ni/ibuffer-vc-hook)
    (add-hook 'ibuffer-hook #'j0ni/ibuffer-vc-hook)


<a id="org7e838f9"></a>

### Key chords

    (key-chord-mode 1)

    (with-eval-after-load 'key-chord
      (key-chord-define-global "df" #'previous-window-any-frame)
      (key-chord-define-global "jk" #'next-window-any-frame)
      (key-chord-define-global ";'" #'j0ni/unicode-shortcut-map)
      (key-chord-define prog-mode-map "[]" #'display-line-numbers-mode))


<a id="orgeaeffb0"></a>

### Flymake

    ;;; Flymake

    ;; (require 'flymake)
    ;; (setq flymake-fringe-indicator-position 'right-fringe)
    ;; (setq flymake-no-changes-timeout nil)
    ;; (setq flymake-start-on-flymake-mode nil)
    ;; (setq flymake-start-on-save-buffer nil)
    ;; (add-hook 'prog-mode-hook #'flymake-mode-on)


<a id="org67e6a88"></a>

### Flycheck

    (setq flycheck-indication-mode 'right-fringe)
    (setq flycheck-checker-error-threshold nil)
    (setq flycheck-idle-change-delay 10.0)
    (setq flycheck-display-errors-delay 10.0)
    (setq flycheck-idle-buffer-switch-delay 10.0)
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (add-hook 'prog-mode-hook #'flycheck-mode)

    (require 'consult-flycheck)


<a id="org651a463"></a>

### Consult

Consult - handy featureful commands, sometimes too noisy

    (require 'consult)

    (consult-customize
     consult-ripgrep consult-git-grep consult-grep consult-theme consult-buffer
     consult-bookmark consult-recent-file consult-xref consult-locate
     consult--source-recent-file consult--source-project-recent-file
     consult--source-bookmark
     :preview-key (kbd "M-.")
     :group nil)

    ;; default value
    (setq consult-async-min-input 3)

    ;; search map
    (dolist (binding '(;; search map
                       ("M-s f" . consult-find)
                       ("M-s F" . consult-locate)
                       ("M-s g" . consult-grep)
                       ("M-s G" . consult-git-grep)
                       ("M-s r" . consult-ripgrep)
                       ("M-s l" . consult-line)
                       ("M-s L" . consult-line-multi)
                       ("M-s m" . consult-multi-occur)
                       ("M-s k" . consult-keep-lines)
                       ("M-s u" . consult-focus-lines)
                       ;; goto map
                       ;; ("M-g e" . consult-compile-error)
                       ("M-g f" . consult-flycheck)
                       ;; ("M-g g" . consult-goto-line)
                       ;; ("M-g M-g" . consult-goto-line)
                       ("M-g o" . consult-org-heading)
                       ;; ("M-g m" . consult-mark)
                       ;; ("M-g k" . consult-global-mark)
                       ("M-g i" . consult-imenu)
                       ("M-g I" . consult-imenu-multi)
                       ;; ("M-s e" . consult-isearch-history)
                       ;; extras, which stomp on command commands
                       ("C-c h" . consult-history)
                       ("C-c m" . consult-mode-command)
                       ("C-c b" . consult-bookmark)
                       ("C-c k" . consult-kmacro)
                       ;; C-x bindings (ctl-x-map)
                       ("C-x M-:" . consult-complex-command)
                       ("C-x b" . consult-buffer)
                       ("C-x 4 b" . consult-buffer-other-window)
                       ("C-x 5 b" . consult-buffer-other-frame)
                       ;; no idea what registers are for, I will read about it :P
                       ("M-#" . consult-register-load)
                       ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
                       ("C-M-#" . consult-register)
                       ))
      (keymap-global-set (car binding) (cdr binding)))

    ;; this is better than isearch
    (keymap-global-set "C-s" #'consult-line)

    (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)
    ;; find the project root
    (with-eval-after-load 'project
      (setq consult-project-root-function (lambda () (cdr (project-current)))))
    ;; when multiple result types are collected in one completion set, hit this key
    ;; to subset to only those of the type at point.
    (setq consult-narrow-key "<")


<a id="orgecd2bad"></a>

### LSP

Language Server Protocol, a Microsoft invention, is providing a common interface for a bunch of languages that are otherwise not so well supported. It's also proving useful in some other well supported modes like clojure and rust.

    (setq lsp-keymap-prefix "C-c l")

    (require 'lsp-mode)

    (require 'lsp-ui)
    (require 'consult-lsp)

    (setq lsp-ui-sideline-delay 2.0)

    (add-hook 'lsp-managed-mode-hook
              (lambda ()
                (setq-local flycheck-checker-error-threshold nil)
                (setq-local flycheck-idle-change-delay 10.0)
                (setq-local flycheck-display-errors-delay 10.0)
                (setq-local flycheck-idle-buffer-switch-delay 10.0)
                ;; turn off idle highlight, let lsp do it...maybe
                (setq-local idle-highlight-timer nil)
                ;; default is t
                (setq-local lsp-enable-folding nil)
                ;; default is t
                (setq-local lsp-eldoc-enable-hover t)
                ;; default is t
                (setq-local lsp-enable-on-type-formatting t)
                ;; default is t
                (setq-local lsp-before-save-edits t)
                ;; default is t
                (setq-local lsp-completion-enable t)
                ;; default is t
                (setq-local lsp-enable-symbol-highlighting t)))


<a id="orgc4e324f"></a>

### Find File in Project

ffip setup

    (require 'find-file-in-project)
    (setq ffip-use-rust-fd t)
    (keymap-global-set "C-c f" #'find-file-in-project-by-selected)


<a id="orga0aefc3"></a>

### IRC - ERC and RCIRC

1.  Shared config

        (defvar j0ni/irc-auth-spec nil)
        (setq j0ni/srht-sasl-pass
              (funcall (plist-get (car (auth-source-search :host "chat.sr.ht")) :secret)))

2.  rcirc

        (require 'rcirc)
        (setq rcirc-debug-flag t)
        (setq rcirc-server-alist
              '(("chat.sr.ht"
                 :nick "joni"
                 :user-name "j0ni@tynan-rcirc"
                 :full-name "Joni Voidshrieker"
                 :port 6697
                 :encryption tls
                 :channels nil)))

        (setq rcirc-authinfo `(("chat.sr.ht" sasl "joni" ,j0ni/srht-sasl-pass)))

        (defun rcirc-handler-AUTHENTICATE (process _cmd _args _text)
          "Respond to authentication request.
        PROCESS is the process object for the current connection."
          (rcirc-send-string
           process
           "AUTHENTICATE"
           (base64-encode-string
            ;; use connection user-name
            (concat "\0" (nth 3 rcirc-connection-info)
                    "\0" (rcirc-get-server-password rcirc-server))
            t)))

        (defun j0ni/rcirc-remove-suffix (STR)
          "Remove suffixes from STR."
          (save-match-data
            (if (string-match "/[[:alpha:]]+?\\'" str)
                (substring str 0 (match-beginning 0))
              str)))

        (setq rcirc-nick-filter #'identity)
        (setq rcirc-channel-filter #'identity)

3.  erc

    ERC, needs a patch for sasl

        (require 'erc)
        (require 'erc-sasl)
        (require 'erc-imenu)
        (require 'bandali-erc)

        (setq erc-format-query-as-channel-p t)
        (setq erc-current-nick-highlight-type 'nick)
        (setq erc-keywords '())
        (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
        (setq erc-track-use-faces t)
        (setq erc-track-faces-priority-list
              '(erc-current-nick-face erc-keyword-face))
        (setq erc-track-priority-faces-only 'all)
        (setq erc-email-userid "j0ni@tynan-erc/irc.libera.chat")

        (defun j0ni/connect-srht-bouncer ()
          (interactive)
          (erc-tls
           :server "chat.sr.ht"
           :port "6697"
           :nick "j0ni"
           :full-name "Joni"
           :password j0ni/srht-sasl-pass))


<a id="org08fede6"></a>

### Undo-fu

undo-fu, ripped from doom

    (setq undo-fu-allow-undo-in-region t)
    (dolist (binding
             `(("C-_"    . ,#'undo-fu-only-undo)
               ("C-/"    . ,#'undo-fu-only-undo)
               ("C-z"    . ,#'undo-fu-only-undo)
               ("<undo>" . ,#'undo-fu-only-undo)
               ("C-x u"  . ,#'undo-fu-only-undo)
               ("M-_"    . ,#'undo-fu-only-redo)
               ("C-M-z"  . ,#'undo-fu-only-redo)))
      (keymap-global-set (car binding) (cdr binding)))

    (global-undo-fu-session-mode 1)


<a id="org4417fbe"></a>

### exec-path-from-shell

This is a bit clumsy, but it works

    (defvar j0ni/exec-path-from-shell-completed nil "Stop this happening repeatedly.")
    (when (and (not j0ni/exec-path-from-shell-completed)
               (memq window-system '(mac ns x pgtk)))
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
      (setq j0ni/exec-path-from-shell-completed t))


<a id="org65d2890"></a>

### Highlight TODO Mode

    (global-hl-todo-mode 1)


<a id="org0be92ef"></a>

### Volatile Highlights

    (volatile-highlights-mode 1)


<a id="orgff044aa"></a>

### Themes!

1.  Modus Themes

    By Prot the Spectacular.

        (require 'modus-themes)

        (setq modus-themes-bold-constructs t)
        (setq modus-themes-italic-constructs nil)
        ;; (setq modus-themes-syntax '(yellow-comments))
        (setq modus-themes-syntax '(faint))
        (setq modus-themes-fringes nil)
        (setq modus-themes-hl-line '(underline neutral))
        (setq modus-themes-completions 'opinionated)
        (setq modus-themes-scale-headings t)
        (setq modus-themes-mode-line '(accented))
        (setq modus-themes-paren-match '(intense bold underline))

        (modus-themes-load-themes)

        ;; (load-theme 'modus-operandi t)
        ;; (load-theme 'modus-vivendi t)

2.  The Matrix

    Weirdly, this is speaking to me at the moment.

        (require 'the-matrix-theme)
        (load-theme 'the-matrix t)


<a id="orgcd40cdf"></a>

### Rainbow Mode

This is for turning the background of all the color strings (e.g. "#ff3700") into the actual color which is IMMENSELY helpful but only when you need it. Otherwise it is awful, and pulls you right out of flow.

    (keymap-global-set "C-c r" #'rainbow-mode)


<a id="org7af2676"></a>

### Rainbow Delimiters Mode

This on the other hand is super useful inside of any lisp code - most of the time themes make good use of it.

    (add-hook 'paredit-mode-hook #'rainbow-delimiters-mode)


<a id="org563ab4f"></a>

### Diff Highlight Mode

This provides better functionality than the various git gutters, and also makes use of vc and integrates with magit. What's not to love. Well, the live version can sometimes slow typing responsiveness right down, so leave that switched off.

    (global-diff-hl-mode 1)


<a id="org27f469e"></a>

### Git Time Machine

This can be useful, but not often enough to have a binding.

    (require 'git-timemachine)


<a id="orgb181a98"></a>

### Expand Region

Super simple alternative to text objects, that vim users go on about.

    (keymap-global-set "C-x C-x" #'er/expand-region)


<a id="orgc9c9e56"></a>

### Anzu

For counting isearch results - mode-line highlighter.

    (global-anzu-mode 1)


<a id="orgd36d6f8"></a>

### Browse Kill Ring

    (browse-kill-ring-default-keybindings)


<a id="orge2464d0"></a>

### Magit

    (setq magit-diff-refine-hunk t)
    (setq magit-bury-buffer-function #'magit-mode-quit-window)

    (keymap-global-set "C-x g" #'magit-status)
    (keymap-global-set "C-x M-g" #'magit-dispatch-popup)

Diff Highlight Mode loaded when the global mode is enabled above. Magit hopefully won't load until first invoked.

    (with-eval-after-load 'magit
      (progn
        (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
        (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))


<a id="orgd956636"></a>

### Idle highlight mode

    (add-hook 'prog-mode-hook #'idle-highlight)


<a id="orga9e9753"></a>

### Paredit

Because it may be ass code but it is the best at what it does.

;;; Useful knowledge, might deserve some extra binds

;; C-M-n forward-list Move forward over a parenthetical group
;; C-M-p backward-list Move backward over a parenthetical group
;; C-M-f forward-sexp Move forward over a balanced expression
;; C-M-b backward-sexp Move backward over a balanced expression
;; C-M-k kill-sexp Kill balanced expression forward
;; C-M-SPC mark-sexp Put the mark at the end of the sexp.

    ;; yer basic lisps
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
    (add-hook 'lisp-mode-hook #'paredit-mode)
    (add-hook 'scheme-mode-hook #'paredit-mode)

    (with-eval-after-load 'paredit
      (progn
        (keymap-set paredit-mode-map "C-M-s" #'paredit-splice-sexp)
        (keymap-set paredit-mode-map "M-s" nil)))

Other modes are hooked in their own configurations.


<a id="org9727a5d"></a>

### Scheme

1.  Geiser

    This is a general purpose slime/sly-ish mode for schemes.

        (add-hook 'scheme-mode-hook #'geiser-mode)
        (add-hook 'geiser-repl-mode-hook #'paredit-mode)

        (require 'geiser-chez)
        (require 'geiser-chicken)

2.  Racket

    Racket on the other hand does much better with its own mode and its builtin repl. So we don't hook it for geiser mode, nor do we load the geiser plugin.

        (add-hook 'racket-mode-hook #'paredit-mode)


<a id="org911dc70"></a>

### Which Key

Pop up a minibuffer help window thingy with key binds in it after pausing for a couple seconds post mod prefix.

    (which-key-mode 1)


<a id="org30fc608"></a>

### Window Switcher

Like avy, but a bit smaller? Or something.

    (require 'switch-window)
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-shortcut-appearance 'text)
    (setq switch-window-auto-resize-window nil)
    (setq switch-window-background t)
    (setq switch-window-default-window-size 0.8)
    (switch-window-mouse-mode 1)
    (keymap-global-set "C-x o" #'switch-window)


<a id="orgd5dbd7c"></a>

### Web mode and webbish stuff

Some of the shit we just have to have, unsightly though it may be.

    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-js-indent-offset 2)
    (setq web-mode-script-padding 0)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.rjs\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))


<a id="orgdfe11d9"></a>

### Common Lisp - Sly

For common lisp, this is most likely the successor to slime.

    (setq sly-default-lisp "sbcl")
    (setq inferior-lisp-program "sbcl")


<a id="org9dc2752"></a>

### Clojure

OK, I recently acquired this knowledge for switching between cider and inf-clojure without having to comment things out and restart, so here are the functions for unplugging whatever is in first.

I should spend some time generalizing this into a toggle.

    (defun j0ni/unhook-cider ()
      "Use this to unfuck clojure buffers when switching live from
    CIDER to inf-clojure."
      (interactive)
      (remove-hook 'clojure-mode-hook #'cider-mode)
      (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
      (seq-doseq (buffer (buffer-list))
        (with-current-buffer buffer
          (when (bound-and-true-p cider-mode)
            (cider-mode -1)
            (inf-clojure-minor-mode 1)))))

    (defun j0ni/unhook-inf-clojure ()
      "Use this to unfuck clojure buffers when switching live from
    inf-clojure to CIDER."
      (interactive)
      (remove-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
      (add-hook 'clojure-mode-hook #'cider-mode)
      (seq-doseq (buffer (buffer-list))
        (with-current-buffer buffer
          (when (bound-and-true-p inf-clojure-minor-mode)
            (inf-clojure-minor-mode -1)
            (cider-mode 1)))))

Some harmless inf-clojure setup

    (add-hook 'inf-clojure-mode-hook #'turn-on-eldoc-mode)
    (add-hook 'inf-clojure-mode-hook #'paredit-mode)

Start off with cider for now.

    (dolist (hook '(clojure-mode-hook
                    clojurec-mode-hook
                    clojurescript-mode-hook
                    clojurex-mode-hook))
      (add-hook hook #'cider-mode)
      (add-hook hook #'paredit-mode)
      (add-hook hook #'subword-mode)
      (add-hook hook #'lsp))


<a id="org6d413d0"></a>

### Lua and Fennel

    (setq monroe-detail-stacktraces t)

    (add-hook 'fennel-mode-hook #'monroe-interaction-mode)
    (add-hook 'fennel-mode-hook #'paredit-mode)


<a id="org128ae96"></a>

### Ruby

    (add-hook 'ruby-mode-hook #'flycheck-mode)
    (setq rbenv-show-active-ruby-in-modeline nil)
    (global-rbenv-mode 1)
    (add-hook 'ruby-mode-hook #'rbenv-use-corresponding)


<a id="org9a67f01"></a>

### C/C++

I spent a lot more time on this than I ever spent writing C or C++.

    (require 'ggtags)
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
                  (setq-local hippie-expand-try-functions-list
                              (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))
                  (ggtags-mode 1))))

    (keymap-set ggtags-mode-map "C-c g s" 'ggtags-find-other-symbol)
    (keymap-set ggtags-mode-map "C-c g h" 'ggtags-view-tag-history)
    (keymap-set ggtags-mode-map "C-c g r" 'ggtags-find-reference)
    (keymap-set ggtags-mode-map "C-c g f" 'ggtags-find-file)
    (keymap-set ggtags-mode-map "C-c g c" 'ggtags-create-tags)
    (keymap-set ggtags-mode-map "C-c g u" 'ggtags-update-tags)

    (keymap-set ggtags-mode-map "M-," 'pop-tag-mark)


<a id="org287bec0"></a>

### Markdown

    (add-hook 'markdown-mode-hook #'visual-line-mode)


<a id="orgf73efcc"></a>

### Purescript

    (add-hook 'purescript-mode-hook #'turn-on-purescript-indentation)
    (add-hook 'purescript-mode-hook #'psc-ide-mode)


<a id="org1207ece"></a>

### Typescript

    (add-hook 'typescript-mode-hook #'lsp)


<a id="org8f04766"></a>

### Evaluation overlays

This renders eval results in-buffer at the end of the eval'd expression. Honestly I've forgotten what life was like before this feature.

    (eros-mode 1)


<a id="org438a553"></a>

### Rust

I am loving this language more and more.

    (add-hook 'rustic-mode-hook #'electric-pair-local-mode)

    (setq rust-indent-method-chain nil)

    (setq rustic-format-trigger nil)
    (setq rustic-lsp-server 'rust-analyzer)
    (setq rustic-lsp-format nil)
    (setq rustic-lsp-client 'lsp-mode)

    (rustic-flycheck-setup)


<a id="orgc41f1fe"></a>

### Org Mode

Org was installed and required before tangling this file, but I believe we can spare a duplicate, since it is a caching operation (or it better be).

    (require 'org)
    (require 'org-agenda)
    (require 'org-clock)


    (keymap-global-set "C-c c" #'org-capture)
    (keymap-global-set "C-c a" #'org-agenda)

    (setq org-startup-indented t)

    ;; fix up encryption - not sure I want this
    ;; (org-crypt-use-before-save-magic)

    ;; make it short to start with
    (setq org-startup-folded t)

    ;; where things live
    (setq org-directory "~/Dropbox/OrgMode/")

    ;; Set agenda file(s)
    (setq org-agenda-files (list (expand-file-name "void.org" org-directory)
                                 (expand-file-name "org-roam" org-directory)
                                 (expand-file-name "berlin.org" org-directory)
                                 (expand-file-name "shrieks.org" org-directory)
                                 (expand-file-name "journal.org" org-directory)))
    (setq org-agenda-span 14)

    ;; Since the very beginning I've had this, to address a problem I no longer
    ;; have: prevent org-mode hijacking arrow keys so I can navigate the buffer
    ;; using arrow keys. So lets not, and see how it goes.
    ;; (setq org-replace-disputed-keys t)

    ;; set our own todo keywords
    (setq org-todo-keywords
          '((sequence "TODO(t!)" "WAITING(w!)" "PAUSED(p!)" "|" "DONE(d@)" "ABANDONED(a@)")))

    (setq org-tag-persistent-alist
          '((home . ?h)
            (sanity . ?s)
            (rachel . ?r)
            (lauren . ?l)
            (ari . ?a)
            (grace . ?g)
            (family . ?f)
            (self . ?m)))

    ;; switch quickly
    (setq org-use-fast-todo-selection 'expert)
    (setq org-priority-default ?C)
    (setq org-log-done 'note)
    (setq org-log-into-drawer t)
    (setq org-special-ctrl-a/e t)
    (setq org-special-ctrl-k t)
    (setq org-use-speed-commands t)
    (setq org-clock-persist t)

    ;; extra indentation
    (setq org-adapt-indentation t)

    ;; Let's have pretty source code blocks
    (setq org-src-preserve-indentation t)

    ;; This is ignored if `org-src-preserve-indentation` is set
    ;; (setq org-edit-src-content-indentation 0)

    (setq org-src-tab-acts-natively t)
    (setq org-src-fontify-natively t)
    (setq org-confirm-babel-evaluate nil)
    (setq org-default-notes-file (concat org-directory "/void.org"))
    (setq org-capture-templates
          `(("j" "Journal" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
             "* %T\n%?\n\n%a")
            ("s" "Shriek" entry (file+headline ,(concat org-directory "/shrieks.org") "Shrieks")
             "* %T\n%?\n")
            ("t" "Task" entry (file+headline ,(concat org-directory "/void.org") "Inbox")
             "* TODO %?\n  %a\n%i")
            ("b" "BP Journal" entry (file+olp+datetree ,(concat org-directory "/bp.org") "Blood Pressure")
             "* %T\n** Systolic: %^{systolic}\n** Diastolic: %^{diastolic}\n** Pulse: %^{pulse}\n** Notes\n%?\n")))

    (defun j0ni/org-mode-hook ()
      ;; org exporting stuff
      (require 'ox-publish)
      ;; org-capture - for inserting into date based trees
      (require 'org-datetree)
      ;; needed for structure templates (<s-TAB etc)
      (require 'org-tempo)
      (org-clock-persistence-insinuate)
      (visual-line-mode 1)
      (add-hook 'before-save-hook 'org-update-all-dblocks nil 'local-only))

    (add-hook 'org-mode-hook #'j0ni/org-mode-hook)
    (add-hook 'org-capture-mode-hook #'j0ni/org-mode-hook)

    (require 'org-habit)

1.  Org publish configuration

        (setq org-publish-project-alist
              `(("notwithstanding"
                 :base-directory ,user-emacs-directory
                 :publishing-directory ,user-emacs-directory
                 :publishing-function org-md-publish-to-md)))

2.  Super Agenda \o/

        (setq org-super-agenda-groups '((:auto-dir-name t)))
        (add-hook 'org-agenda-mode-hook #'org-super-agenda-mode)

3.  Org Roam

        ;; (setq org-roam-v2-ack t)
        (setq org-roam-directory (expand-file-name "org-roam" org-directory))

        (org-roam-db-autosync-mode 1)


<a id="org070b716"></a>

### ELFeed - RSS Reader

    (setq elfeed-feeds '("https://pluralistic.net/feed/"
                         "https://theguardian.com/rss"
                         "https://www.space.com/feeds/all"
                         "https://www.sciencedaily.com/rss/all.xml"
                         "https://spectrum.ieee.org/feeds/feed.rss"
                         "https://journals.plos.org/plosbiology/feed/atom"
                         "http://feeds.feedburner.com/pnas/UJrK?format=xml"
                         "https://www.alternet.org/feeds/feed.rss"
                         "https://www.democracynow.org/democracynow.rss"
                         "https://www.anarchistnews.org/rss.xml"
                         "https://www.anarchistfederation.net/feed/"
                         "https://www.no-gods-no-masters.com/blog/rss"
                         "https://taz.de/!p4608;rss/"
                         "https://taz.de/Schwerpunkt-Klimawandel/!t5008262;rss/"))


<a id="orgf3ad7fe"></a>

### Telega

Because of course Telegram in Emacs, in Russian.

    (require 'telega)
    (add-hook 'telega-chat-mode-hook #'visual-line-mode)
    (add-hook 'telega-chat-mode-hook #'telega-mode-line-mode)
    (add-hook 'telega-chat-mode-hook #'telega-notifications-mode)


<a id="org1ed1c36"></a>

### Minions

Remove the annoying mode list

    (minions-mode 1)
    (keymap-global-set "C-x C-m" #'minions-minor-modes-menu)


<a id="org585b542"></a>

### Icons

Icons for noisy modes, milk for the morning cake.

    (eval-when-compile
      '(all-the-icons-install-fonts))


<a id="orge23d95a"></a>

### Haskell

    (add-hook 'haskell-mode-hook #'electric-pair-mode)
    (add-hook 'haskell-mode-hook #'subword-mode)
    (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook #'haskell-doc-mode)


<a id="orge57d0a7"></a>

### Olivetti Mode

Declutter the screen - good for big screens, laptop doesn't care.

    (setq olivetti-body-width 120)


<a id="orgd40b02f"></a>

### Move Text

I remember this from Netbeans!

    (keymap-global-set "M-S-<up>" #'move-text-up)
    (keymap-global-set "M-S-<down>" #'move-text-down)


<a id="org9071105"></a>

### Set all the fonts one last time

    (j0ni/init-frame)


<a id="orge0f3941"></a>

### Mu 4 Emacs

Mu4e isn't packaged in the usual way, it gets installed as part of the \`mu\` system package, or I install it from source.

Either way, this is flaky as hell and almost always needs tweaking for a new OS. I should make a more generic function to prioritize possible locations and pick the first it finds. TODO

    (defvar j0ni/mu4e-path nil "Find a mu4e client to run")

    (if j0ni/is-mac
        (setq j0ni/mu4e-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
      (setq j0ni/mu4e-path "/usr/local/share/emacs/site-lisp/mu4e"))

    (add-to-list 'load-path j0ni/mu4e-path)

    (require 'mu4e)

    (defun j0ni/mu4e-bookmark (sub-maildir days char)
      (list (concat "date:" days "d..now AND (maildir:/" sub-maildir
                    "/INBOX OR maildir:/" sub-maildir "/sent-mail) AND NOT flag:trashed")
            (concat "Last " days " days (" sub-maildir ")")
            char))

    (setq mu4e-decryption-policy t
          mu4e-update-interval nil
          mu4e-index-update-in-background nil
          mu4e-get-mail-command "true"
          mu4e-hide-index-messages t
          mu4e-confirm-quit nil
          mu4e-use-fancy-chars nil ;; they actually look shit
          mu4e-headers-sort-direction 'ascending
          mu4e-headers-skip-duplicates t
          mu4e-change-filenames-when-moving t
          mu4e-headers-hide-predicate nil
          mu4e-headers-include-related t
          mu4e-split-view nil
          mu4e-headers-fields '((:human-date . 12)
                                (:flags . 6)
                                (:mailing-list . 16)
                                (:from-or-to . 25)
                                (:thread-subject))
          mu4e-compose-complete-only-after "2012-01-01"
          mu4e-compose-signature "In this world / we walk on the roof of hell / gazing at flowers\n    - Kobayashi Issa\n\nhttps://j0ni.ca ~ https://keybase.io/j0ni"
          mu4e-view-show-addresses t
          mm-inline-large-images 'resize
          message-send-mail-function 'smtpmail-send-it
          sendmail-program "/usr/bin/msmtp"
          message-sendmail-f-is-evil t
          message-sendmail-extra-arguments '("--read-envelope-from")
          message-kill-buffer-on-exit t
          mail-user-agent 'mu4e-user-agent
          message-citation-line-function 'message-insert-formatted-citation-line
          message-citation-line-format "On %a, %d %b %Y at %T %z, %f wrote:"
          mu4e-personal-addresses '("j@lollyshouse.ca"
                                    "hi@mhcat.ca"
                                    "jonathan.irving@gmail.com"
                                    "j0ni@fastmail.com"
                                    "joni@well.com"
                                    "j0ni@protonmail.com"
                                    "jon@arity.ca")
          mml-secure-openpgp-signers '("D6346AC6D110409636A0DBF4F7F645B8CE3F8FA3")
          mml-secure-openpgp-sign-with-sender nil
          mu4e-context-policy 'pick-first
          mu4e-contexts
          (list (make-mu4e-context
                 :name "Fastmail"
                 :enter-func (lambda ()
                               (when (mu4e-running-p)
                                 (mu4e-update-mail-and-index nil))
                               (mu4e-message "Switching to Fastmail context"))
                 :match-func (lambda (msg)
                               (when msg
                                 (string-match-p "^/Fastmail" (mu4e-message-field msg :maildir))))
                 :vars `((user-mail-address . "j@lollyshouse.ca")
                         (user-full-name . "Jon Irving")
                         (mu4e-sent-messages-behavior . sent)
                         (mu4e-sent-folder . "/Fastmail/sent-mail")
                         (mu4e-trash-folder . "/Fastmail/trash")
                         (mu4e-drafts-folder . "/Fastmail/drafts")
                         (mu4e-refile-folder . "/Fastmail/all-mail")
                         (mu4e-maildir-shortcuts . (("/Fastmail/INBOX" . ?i)
                                                    ("/Fastmail/sent-mail" . ?s)
                                                    ("/Fastmail/drafts" . ?d)
                                                    ("/Fastmail/trash" . ?t)))
                         (mu4e-bookmarks . ,(list (j0ni/mu4e-bookmark "Fastmail" "7" ?w)
                                                  (j0ni/mu4e-bookmark "Fastmail" "30" ?m)))
                         (smtpmail-smtp-user . "j0ni@fastmail.com")
                         (smtpmail-smtp-server . "smtp.fastmail.com")
                         (smtpmail-smtp-service . 587)
                         (smtpmail-stream-type . starttls)))
                (make-mu4e-context
                 :name "Well"
                 :enter-func (lambda ()
                               (when (mu4e-running-p)
                                 (mu4e-update-mail-and-index nil))
                               (mu4e-message "Switching to the Well context"))
                 :match-func (lambda (msg)
                               (when msg
                                 (string-match-p "^/Well" (mu4e-message-field msg :maildir))))
                 :vars `((user-mail-address . "joni@well.com")
                         (user-full-name . "Jon Irving")
                         (mu4e-sent-messages-behavior . sent)
                         (mu4e-sent-folder . "/Well/Sent")
                         (mu4e-trash-folder . "/Well/Trash")
                         (mu4e-drafts-folder . "/Well/Drafts")
                         (mu4e-refile-folder . "/Well/Archive")
                         (mu4e-maildir-shortcuts . (("/Well/INBOX" . ?i)
                                                    ("/Well/Sent" . ?s)
                                                    ("/Well/Drafts" . ?d)
                                                    ("/Well/Trash" . ?t)
                                                    ("/Well/Archive" . ?a)))
                         (mu4e-bookmarks . ,(list (j0ni/mu4e-bookmark "Well" "7" ?w)
                                                  (j0ni/mu4e-bookmark "Well" "30" ?m)))
                         (smtpmail-smtp-user . "joni")
                         (smtpmail-smtp-server . "iris.well.com")
                         (smtpmail-smtp-service . 587)
                         (smtpmail-stream-type . starttls)))))

    (add-hook 'message-mode-hook #'turn-on-auto-fill)
    (add-hook 'message-mode-hook #'mml-secure-message-sign-pgpmime)


<a id="org3b6a0c5"></a>

### Crypto setup

    (setq auth-source-debug t)
    (epa-file-enable)

    (setenv "GPG_AGENT_INFO" nil) ;; use emacs pinentry

    (setq epa-pinentry-mode 'loopback)
    (setq epg-pinentry-mode 'loopback)

    (pinentry-start t) ;; don't complain if its already running


<a id="orgd9d6e1f"></a>

### Custom file configuration

So this is all done declaratively above using setq. However there is an advantage to using the customization feature when packages declare defcustoms, and that is that there can be callbacks associated with setting a customization. It may be that I go back to a use-package based config, in which case I will switch all the customizations to the :custom keyword settings, which is the best of both worlds.

Or, I might figure out how to manually use the configuration macros - but I suspect they need to occur literally once only, so I'd need a huge form wrapped around all this.

    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

    (when (file-exists-p custom-file)
      ;; don't (load custom-file)
      (warn "There are customization settings in custom.el - give it a gander"))
