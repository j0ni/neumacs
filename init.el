;;; init.el --- emacs config -*- no-byte-compile: t ; lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; All of the things in one file, with exceptions. First, the packaging system
;;; gets bootstrapped in the boot.el file. Second, some key bindings have their
;;; own file, and more will probably move there. Third, eventually hooks, which
;;; join packages together, will go into hooks.el.
;;;
;;; Code:

;; (debug-watch 'indent-tabs-mode)

;; (desktop-save-mode)

(j0ni/init-frame)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'wm)
;; (j0ni/exwm-enable)

(require 'boot)
(require 'keys)

(defvar j0ni/is-mac (memq window-system '(mac ns)))

;; whitespace-mode
(setq whitespace-line-column 100)
(setq whitespace-style '(face trailing lines-tail tabs))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; conf files
(add-hook 'conf-mode-hook #'electric-pair-local-mode)

;; main emacs setup
(setq warning-suppress-types '((comp)))
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
;; 43.67066, -79.30211 - location
;; (setq calendar-longitude 43.67066)
;; (setq calendar-latitude -79.30211)
;; (setq calendar-location-name "Toronto")

;; OS dependent modifier setup
(when j0ni/is-mac
  ;; The left and right Option or Alt keys.
  (setq ns-alternate-modifier 'meta)
  (setq ns-right-alternate-modifier 'left)
  ;; The left and right Command keys.
  (setq ns-command-modifier 'meta)
  (setq ns-right-command-modifier 'super)
  ;; The Function (fn) key.
  (setq ns-function-modifier 'none))

(setq-default browse-url-browser-function
              (cl-case system-type
                ((darwin macos) 'browse-url-default-macosx-browser)
                (t 'browse-url-default-browser)))

(defalias 'yes-or-no-p 'y-or-n-p)

(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; Fonty fonty fonty fonty fonty LEAVE ME ALONE fonty fonty fonty
;; fonty has moved to early-init

;; adds a little space with some fonts
(setq scroll-step 0)
(setq scroll-margin 2)
(setq auto-window-vscroll nil)
(pixel-scroll-precision-mode 1)

;; be sure to set this to 0 in any auto-scrolling buffers
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position t)

;; apparently the default is pretty small - like around 1MB
(setq gc-cons-threshold (* 100 1024 1024))

(setq create-lockfiles nil)
(setq redisplay-dont-pause t)
(setq disabled-command-function nil)
(setq ring-bell-function 'ignore)
(setq next-screen-context-lines 3)

;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(setq require-final-newline t)

(delete-selection-mode 1)

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
  (load-file (expand-file-name "init.el" user-emacs-directory)))

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

(defun j0ni/keymap-set (map k command)
  (let ((k (if (stringp k) (kbd k) k)))
    (keymap-set map k command)))

;; because I honestly don't care about anyone else
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; start a bunch of global modes
(dolist (mode '(electric-indent-mode
                show-paren-mode
                save-place-mode
                size-indication-mode
                ;; global-hl-line-mode
                column-number-mode
                winner-mode
                global-auto-revert-mode
                recentf-mode))
  (funcall mode 1))

(blink-cursor-mode -1)
(remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
(advice-add #'shr-colorize-region :around (defun shr-no-colorise-region (&rest ignore)))

;;; Dired

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j (dired-jump)
(require 'dired-x)

;; a nice process editor - who knew (everyone but me no doubt)
(keymap-global-set "C-x P" #'proced)

(keymap-set lisp-mode-shared-map "C-c C-k" #'eval-buffer)

(dolist (binding
         '(("C-x C-r" . revert-buffer)
           ("C-x |" . j0ni/toggle-window-split)
           ("C-c ." . j0ni/delete-whitespace)
           ("C-c s" . j0ni/insert-shrug)
           ("C-=" . text-scale-increase)
           ("C--" . text-scale-decrease)))
  (keymap-global-set (car binding) (cdr binding)))

(with-eval-after-load 'key-chord
  (key-chord-define-global "df" #'previous-window-any-frame)
  (key-chord-define-global "jk" #'next-window-any-frame)
  (key-chord-define-global ";'" #'j0ni/unicode-shortcut-map)
  (key-chord-define prog-mode-map "[]" #'display-line-numbers-mode))

;; history
(setq savehist-save-minibuffer-history t)
(setq history-length 10000)
(setq history-delete-duplicates t)

(savehist-mode 1)

;; minibuffer setup
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq minibuffer-completion-confirm 'confirm)
;; [ ... ] instead of (default ...
(setq minibuffer-eldef-shorten-default t)
;; I think this is bad for my impulsive fingers
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)

;; some completion configuration
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-cycle-threshold 3)
(setq completions-detailed t)
(setq completions-format 'one-column)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(minibuffer-electric-default-mode 1)
(file-name-shadow-mode 1)

;; set up time display but don't turn it on
(setq display-time-format nil)
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

;;; Set up xref

(setq xref-marker-ring-length 64)
(setq xref-show-xrefs-function 'xref--show-xref-buffer) ; default
(setq xref-show-definitions-function 'xref-show-definitions-completing-read)

;;; Consult - handy featureful commands, sometimes too noisy

(straight-use-package 'consult)
(require 'consult)

(consult-customize
 consult-ripgrep consult-git-grep consult-grep consult-theme consult-buffer
 consult-bookmark consult-recent-file consult-xref consult-locate
 consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
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
                   ("M-g e" . consult-compile-error)
                   ("M-g f" . consult-flycheck)
                   ("M-g g" . consult-goto-line)
                   ("M-g M-g" . consult-goto-line)
                   ("M-g o" . consult-org-heading)
                   ("M-g m" . consult-mark)
                   ("M-g k" . consult-global-mark)
                   ("M-g i" . consult-imenu)
                   ("M-g I" . consult-imenu-multi)
                   ("M-s e" . consult-isearch-history)
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
                   ("C-M-#" . consult-register)))
  (keymap-global-set (car binding) (cdr binding)))

;; isearch related commands
(dolist (binding '(("M-e" . consult-isearch-history)
                   ("M-s e" . consult-isearch-history)
                   ("M-s l" . consult-line)
                   ("M-s L" . consult-line-multi)))
  (keymap-set isearch-mode-map (car binding) (cdr binding)))

(keymap-global-set "C-s" #'consult-line)

(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add #'register-preview :override #'consult-register-window)
;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref)
(setq xref-show-definitions-function #'consult-xref)
;; find the project root
(with-eval-after-load 'projectile
  (setq consult-project-root-function #'projectile-project-root))
;; when multiple result types are collected in one completion set, hit this key
;; to subset to only those of the type at point.
(setq consult-narrow-key "<")

;;; LSP

(straight-use-package 'lsp-mode)
(require 'lsp-mode)

(straight-use-package 'lsp-ui)
(straight-use-package 'consult-lsp)
(require 'lsp-ui)
(require 'consult-lsp)

;; turn off idle highlight, let lsp do it...maybe
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (setq-local idle-highlight-timer nil)))

;; default is t
(setq lsp-enable-folding nil)
;; default is t
(setq lsp-eldoc-enable-hover t)
;; default is t
(setq lsp-enable-on-type-formatting t)
;; default is t
(setq lsp-before-save-edits t)

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
(setq ibuffer-saved-filter-groups nil)
(setq ibuffer-old-time 48)

(keymap-global-set "C-x C-b" #'ibuffer)

(straight-use-package 'ibuffer-vc)
(require 'ibuffer-vc)

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              vc-relative-file)))

(defun j0ni/ibuffer-vc-hook ()
  (ibuffer-auto-mode 1)
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

;; (remove-hook 'ibuffer-hook #'j0ni/ibuffer-vc-hook)
(add-hook 'ibuffer-hook #'j0ni/ibuffer-vc-hook)

;; ffip setup
(straight-use-package 'find-file-in-project)
(setq ffip-use-rust-fd t)
(keymap-global-set "C-c f" #'find-file-in-project-by-selected)

;; IRC, sigh...
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

(setq rcirc-authinfo
      '(("chat.sr.ht" sasl "joni" "")))

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

;; ERC, needs a patch for sasl
(straight-use-package 'erc)
(require 'erc)
(require 'erc-sasl)
(require 'bandali-erc)

(setq erc-email-userid "j0ni@tynan-erc/irc.libera.chat")

(defun j0ni/connect-srht-bouncer ()
  (interactive)
  (erc-tls
   :server "chat.sr.ht"
   :port "6697"
   :nick "j0ni"
   :full-name "Joni"
   :password ""))

;; undo-fu, ripped from doom
;; (straight-use-package 'undo-fu)
;; (setq undo-fu-allow-undo-in-region t)
;; (dolist (binding
;;          `(("C-_"    . ,#'undo-fu-only-undo)
;;            ("C-/"    . ,#'undo-fu-only-undo)
;;            ("C-z"    . ,#'undo-fu-only-undo)
;;            ("<undo>" . ,#'undo-fu-only-undo)
;;            ("C-x u"  . ,#'undo-fu-only-undo)
;;            ("M-_"    . ,#'undo-fu-only-redo)
;;            ("C-M-z"  . ,#'undo-fu-only-redo)))
;;   (j0ni/global-set-key (car binding) (cdr binding)))

;; (straight-use-package 'undo-fu-session)
;; (global-undo-fu-session-mode 1)

;; This is a bit clumsy, but it works
(straight-use-package 'exec-path-from-shell)
(defvar j0ni/exec-path-from-shell-completed nil "Stop this happening repeatedly")
(when (and (not j0ni/exec-path-from-shell-completed)
           (memq window-system '(mac ns x)))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  (setq j0ni/exec-path-from-shell-completed t))

(straight-use-package 'hl-todo)
(global-hl-todo-mode 1)

(straight-use-package 'volatile-highlights)
(volatile-highlights-mode 1)

;; Themes!
(straight-use-package 'modus-themes)
(require 'modus-themes)

(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs nil)
(setq modus-themes-syntax '(yellow-comments))
(setq modus-themes-fringes nil)
(setq modus-themes-hl-line '(underline neutral))
(setq modus-themes-completions 'opinionated)
(setq modus-themes-scale-headings t)
(setq modus-themes-mode-line '(accented))
(setq modus-themes-paren-match '(intense bold underline))

(modus-themes-load-themes)

;; (load-theme 'modus-operandi t)
(load-theme 'modus-vivendi t)

(straight-use-package 'rainbow-mode)
(keymap-global-set "C-c r" #'rainbow-mode)

(straight-use-package 'rainbow-delimiters)
(add-hook 'paredit-mode-hook #'rainbow-delimiters-mode)

(straight-use-package 'browse-at-remote)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(straight-use-package 'diff-hl)
(global-diff-hl-mode 1)

(with-eval-after-load 'magit
  (progn
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(straight-use-package 'git-timemachine)
(keymap-global-set "C-x C-g" #'git-timemachine)

(straight-use-package 'expand-region)
(keymap-global-set "C-x C-x" #'er/expand-region)

(straight-use-package 'dockerfile-mode)

(straight-use-package 'anzu)
(global-anzu-mode 1)

;;; Completion

(straight-use-package 'vertico)
(require 'vertico)
(setq vertico-cycle t)

(straight-use-package 'yasnippet)
(setq yas-snippet-dirs (concat user-emacs-directory "snippets"))

(straight-use-package 'corfu)
(require 'corfu)

(setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;; (setq corfu-auto t)              ;; Enable auto completion
(setq corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
(setq corfu-quit-at-boundary nil)   ;; Automatically quit at word boundary
(setq corfu-quit-no-match t)        ;; Automatically quit if there is no match
(setq corfu-echo-documentation t)   ;; Do not show documentation in the echo area
(setq corfu-scroll-margin 2)        ;; Use scroll margin
(setq corfu-min-width 40)
(setq corfu-preview-current t)      ;; Preview current candidate

(add-hook 'emacs-startup-hook #'corfu-global-mode)

;; Optionally use TAB for cycling, default is `corfu-complete'.
(with-eval-after-load 'corfu
  (keymap-set corfu-map "<tab>" #'corfu-next)
  (keymap-set corfu-map "<backtab>" #'corfu-previous))

(straight-use-package
 '(cape :type git :host github :repo "minad/cape"))

;; Add `completion-at-point-functions', used by `completion-at-point'.
;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
;;(add-to-list 'completion-at-point-functions #'cape-ispell)
;;(add-to-list 'completion-at-point-functions #'cape-dict)

(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-keyword)
(add-to-list 'completion-at-point-functions #'cape-symbol)
;; (add-to-list 'completion-at-point-functions #'cape-line)

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

(require 'abbrev)
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(straight-use-package 'marginalia)
(setq marginalia-margin-min 10)
(setq marginalia-align-offset 3)
(marginalia-mode 1)

(straight-use-package 'orderless)
(require 'orderless)
(setq orderless-matching-styles '(orderless-literal orderless-regexp))

;; see completion-styles-alist for the defaults, if this turns out to
;; not be quite right.
(setq completion-styles '(orderless))

(orderless-define-completion-style orderless+initialism
  (orderless-matching-styles '(orderless-initialism
                               orderless-literal
                               orderless-regexp)))
(setq completion-category-overrides
      '((command (styles orderless+initialism))
        (symbol (styles orderless+initialism))
        (variable (styles orderless+initialism))
        (buffer (styles orderless+initialism))
        (project-file (styles orderless+initialism))
        (xref-location (styles orderless))
        (symbol-help (styles orderless+initialism))
        (info-menu (styles orderless+initialism))
        (file (styles orderless+initialism))))

(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(require 'embark-consult)
(setq prefix-help-command #'embark-prefix-help-command)
(keymap-global-set "C-." #'embark-act)
(keymap-global-set "C-," #'embark-dwim)
(keymap-global-set "C-h B" #'embark-bindings)

(defun with-minibuffer-keymap (keymap)
  (lambda (fn &rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (use-local-map
           (make-composed-keymap keymap (current-local-map))))
      (apply fn args))))

(defvar embark-completing-read-prompter-map nil)
(setq embark-completing-read-prompter-map
      (let ((map (make-sparse-keymap)))
        (keymap-set map "TAB" #'abort-recursive-edit)
        map))

(advice-add 'embark-completing-read-prompter :around
            (with-minibuffer-keymap embark-completing-read-prompter-map))

(defun embark-act-with-completing-read (&optional arg)
  (interactive "P")
  (let* ((embark-prompter 'embark-completing-read-prompter)
         (act (propertize "Act" 'face 'highlight))
         (embark-indicator (lambda (_keymap targets) nil)))
    (embark-act arg)))

(keymap-set vertico-map "TAB" #'embark-act-with-completing-read)

;; just in case I guess
(with-eval-after-load 'icomplete
  (let ((map icomplete-minibuffer-map))
    (keymap-set map "SPC" nil)
    (keymap-set map "?" nil)
    (keymap-set map "TAB" #'icomplete-force-complete)
    (keymap-set map "C-l" #'embark-collect-live)
    (keymap-set map "M-l" #'embark-collect-snapshot)
    (keymap-set map "M-s" #'embark-collect-snapshot)
    (keymap-set map "C-." #'embark-act)
    (keymap-set map "C-," #'embark-dwim)))

;; This is taken from the embark wiki, I like it better than
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

;;; Vertico

(vertico-mode 1)

;;; Kill ring

(straight-use-package 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;; Magit

(straight-use-package 'magit)
(setq magit-diff-refine-hunk t)
(setq magit-bury-buffer-function #'magit-mode-quit-window)

(keymap-global-set "C-x g" #'magit-status)
(keymap-global-set "C-x M-g" #'magit-dispatch-popup)

;;; Idle highlights

(straight-use-package 'idle-highlight)
(add-hook 'prog-mode-hook #'idle-highlight)

;;; Projectile - various git project narrowed functions

(straight-use-package 'projectile)
(projectile-mode 1)
(keymap-global-set "C-c p" #'projectile-command-map)
(keymap-global-set "C-c C-p" #'projectile-command-map)
(keymap-set projectile-command-map "C-p" #'projectile-switch-project)

(straight-use-package 'projectile-ripgrep)
(grep-apply-setting
 'grep-find-command
 '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))

;;; Useful knowledge, might deserve some extra binds

;; C-M-n forward-list Move forward over a parenthetical group
;; C-M-p backward-list Move backward over a parenthetical group
;; C-M-f forward-sexp Move forward over a balanced expression
;; C-M-b backward-sexp Move backward over a balanced expression
;; C-M-k kill-sexp Kill balanced expression forward
;; C-M-SPC mark-sexp Put the mark at the end of the sexp.

;;; Paredit, always Paredit
(straight-use-package 'paredit)
;; yer basic lisps
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'scheme-mode-hook #'paredit-mode)

(with-eval-after-load 'paredit
  (progn
    (keymap-set paredit-mode-map "C-M-s" #'paredit-splice-sexp)
    (keymap-set paredit-mode-map "M-s" nil)))

;;; Scheme
(straight-use-package 'geiser)
(add-hook 'scheme-mode-hook #'geiser-mode)
(add-hook 'geiser-repl-mode-hook #'paredit-mode)

(straight-use-package 'geiser-chicken)
(straight-use-package 'geiser-guile)
(straight-use-package 'geiser-racket)
(straight-use-package 'geiser-chez)

;;; The other Scheme
(straight-use-package 'racket-mode)
(add-hook 'racket-mode-hook #'paredit-mode)
(add-hook 'racket-mode-hook #'geiser-mode)

;;; Some guidance please
(straight-use-package 'which-key)
(which-key-mode 1)

;;; Like avy, but a bit nicer?
(straight-use-package 'switch-window)
(require 'switch-window)
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-shortcut-appearance 'text)
(setq switch-window-auto-resize-window nil)
(setq switch-window-background t)
(setq switch-window-default-window-size 0.8)
(switch-window-mouse-mode 1)
(keymap-global-set "C-x o" #'switch-window)

;;; Some of the shit we just have to have
(straight-use-package 'yaml-mode)
(straight-use-package 'web-mode)
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

(straight-use-package 'restclient)

;;; Common Lisp

(straight-use-package 'sly)
(setq sly-default-lisp "sbcl")
(setq inferior-lisp-program "sbcl")

(straight-use-package 'sly-quicklisp)
(straight-use-package 'sly-macrostep)
(straight-use-package 'sly-asdf)

;;; Clojure setup

(straight-use-package 'inf-clojure)
(add-hook 'inf-clojure-mode-hook #'turn-on-eldoc-mode)
(add-hook 'inf-clojure-mode-hook #'paredit-mode)

(straight-use-package 'clojure-mode)
(dolist (hook '(clojure-mode-hook
                clojurec-mode-hook
                clojurescript-mode-hook
                clojurex-mode-hook))
  (add-hook hook (lambda ()
                   (paredit-mode 1)
                   (inf-clojure-minor-mode 1)
                   (subword-mode 1)
                   (lsp))))

;;; Lua and Fennel

(straight-use-package 'lua-mode)

(straight-use-package 'fennel-mode)
(straight-use-package 'monroe)
(setq monroe-detail-stacktraces t)

(add-hook 'fennel-mode-hook #'monroe-interaction-mode)
(add-hook 'fennel-mode-hook #'paredit-mode)

;;; Ruby

(straight-use-package 'ruby-mode)
(add-hook 'ruby-mode-hook #'flycheck-mode)

(straight-use-package 'inf-ruby)
(straight-use-package 'rbenv)
(setq rbenv-show-active-ruby-in-modeline nil)

(global-rbenv-mode 1)
(add-hook 'ruby-mode-hook #'rbenv-use-corresponding)

;;; Misc

(straight-use-package 'json-mode)
(straight-use-package 'graphql-mode)

(straight-use-package 'markdown-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)

(straight-use-package 'purescript-mode)
(add-hook 'purescript-mode-hook #'turn-on-purescript-indentation)

(straight-use-package 'psc-ide)
(add-hook 'purescript-mode-hook #'psc-ide-mode)

(straight-use-package 'typescript-mode)
(add-hook 'typescript-mode-hook #'lsp)

;;; Eval overlays

(straight-use-package 'eros)
(eros-mode 1)

;;; Flycheck - I tried to use flymake, but it is limited

(straight-use-package 'flycheck)
(straight-use-package 'flycheck-eldev)
(setq flycheck-indication-mode 'right-fringe)
(setq flycheck-checker-error-threshold nil)
(add-hook 'prog-mode-hook #'flycheck-mode)

;;; Flymake

;; (require 'flymake)
;; (setq flymake-fringe-indicator-position 'right-fringe)
;; (setq flymake-no-changes-timeout nil)
;; (setq flymake-start-on-flymake-mode nil)
;; (setq flymake-start-on-save-buffer nil)
;; (add-hook 'prog-mode-hook #'flymake-mode-on)

;;; Rust

(straight-use-package 'rustic)

(add-hook 'rustic-mode-hook #'electric-pair-local-mode)

(setq rust-indent-method-chain nil)

(setq rustic-format-trigger nil)
(setq rustic-lsp-server 'rust-analyzer)
(setq rustic-lsp-format nil)
(setq rustic-lsp-client 'lsp-mode)

(rustic-flycheck-setup)

;;; Org Mode

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

;; prevent org-mode hijacking arrow keys
(setq org-replace-disputed-keys t)

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

(straight-use-package 'simple-httpd)

(straight-use-package 'org-super-agenda)
(setq org-super-agenda-groups '((:auto-dir-name t)))
(add-hook 'org-agenda-mode-hook #'org-super-agenda-mode)

(straight-use-package 'org-roam)
;; (setq org-roam-v2-ack t)
(setq org-roam-directory (expand-file-name "org-roam" org-directory))

(org-roam-db-autosync-mode 1)

(require 'org-habit)

;;; ELFeed

(straight-use-package 'elfeed)
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
                     "https://www.anarchistfederation.net/feed/"))

;;; Telegram

;; (straight-use-package 'telega)
;; (require 'telega)
;; (add-hook 'telega-chat-mode-hook #'visual-line-mode)
;; (add-hook 'telega-chat-mode-hook #'telega-mode-line-mode)
;; (add-hook 'telega-chat-mode-hook #'telega-notifications-mode)

;; (with-eval-after-load 'telega
;;   (when (featurep 'modus-themes)
;;     (defadvice modus-themes-toggle (after clear-telega-icon-cache activate)
;;       (setq telega-mode-line--logo-image-cache nil))))

;;; Remove the annoying mode list

(straight-use-package 'minions)
(minions-mode 1)
(keymap-global-set "C-x C-m" #'minions-minor-modes-menu)

;;; icons for noisy modes

(straight-use-package 'all-the-icons)
(eval-when-compile
  '(all-the-icons-install-fonts))

;;; Haskell

(straight-use-package 'haskell-mode)
(add-hook 'haskell-mode-hook #'electric-pair-mode)
(add-hook 'haskell-mode-hook #'subword-mode)
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)
(add-hook 'haskell-mode-hook #'haskell-doc-mode)

;;; Declutter

(straight-use-package 'olivetti)
(setq olivetti-body-width 120)

;;; I remember this from Netbeans!

(straight-use-package 'move-text)
(keymap-global-set "M-S-<up>" #'move-text-up)
(keymap-global-set "M-S-<down>" #'move-text-down)

;; mu4e isn't packaged in the usual way, it gets installed as part of the `mu` system package.

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

(straight-use-package '2048-game)

;; let's get encryption established - for exwm
(setq auth-source-debug t)
(epa-file-enable)

(setenv "GPG_AGENT_INFO" nil) ;; use emacs pinentry

(straight-use-package 'pinentry)
(setq epa-pinentry-mode 'loopback)
(setq epg-pinentry-mode 'loopback)

(pinentry-start t) ;; don't complain if its already running

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  ;; don't (load custom-file)
  (warn "There are customization settings in custom.el - give it a gander"))
