;;; init.el --- monolithic emacs config -*- no-byte-compile: t -*-
;;;
;;; Commentary:
;;;
;;; All of the things in one file, with exceptions. First, the packaging system
;;; gets bootstrapped in the boot.el file. Second, some key bindings have their
;;; own file, and more will probably move there. Third, eventually hooks, which
;;; join packages together, will go into hooks.el.
;;;
;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'boot)

(use-package emacs
  :hook ((before-save . delete-trailing-whitespace)
         (emacs-lisp . enable-paredit-mode)
         (prog . whitespace-mode))
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (inhibit-startup-screen t)
  (vc-follow-symlinks t)
  (find-file-suppress-same-file-warnings t)
  (read-file-name-completion-ignore-case t)
  (comint-prompt-read-only t)
  ;; This defaults to a warning - abo-abo uses advice quite a bit, which is
  ;; where this comes from; I don't see much in the way of warnings yet so let's
  ;; leave this out for now.
  ;; (ad-redefinition-action 'accept)
  ;; hmm
  (select-enable-clipboard t)
  (select-enable-primary t)
  (uniquify-buffer-name-style 'forward)
  (save-interprogram-paste-before-kill t)
  (compilation-always-kill  t)
  (compilation-ask-about-save nil)
  (apropos-do-all t)
  (mouse-yank-at-point t)
  (save-place-file (concat user-emacs-directory ".places"))
  (backup-directory-alist `(("." . ,(concat user-emacs-directory ".backups"))))
  (enable-local-variables :all)
  (confirm-kill-emacs nil)
  (sentence-end-double-space nil)
  (whitespace-line-column 100)
  (whitespace-style '(face trailing lines-tail tabs))
  (delete-old-versions t)
  (version-control t)
  (custom-safe-themes t)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
  (mouse-wheel-progressive-speed t)              ; don't accelerate scrolling
  (shr-color-visible-luminance-min 80)
  :init
  (setq-default browse-url-browser-function
                (cl-case system-type
                  ((darwin macos) 'browse-url-default-macosx-browser)
                  (t 'browse-url-firefox)))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-frame-font "Iosevka Snuggle-11" t t)
  (set-face-font 'variable-pitch "Lucida Grande-12" nil)
  (set-face-font 'fixed-pitch "Iosevka Snuggle-11" nil)
  (set-face-font 'fixed-pitch-serif "Iosevka Snuggle-11" nil)
  (set-fontset-font t 'unicode "Symbola" nil 'prepend)
  (when (string= system-type "gnu/linux")
    (setq x-super-keysym 'meta))
  (setq scroll-step 0)
  (setq scroll-margin 2)
  (setq auto-window-vscroll nil)
  ;; be sure to set this to 0 in any auto-scrolling buffers
  (setq scroll-conservatively 100000)
  (setq scroll-preserve-screen-position t)
  (setq gc-cons-threshold (* 20 1024 1024))
  (setq auto-revert-verbose nil)
  (setq create-lockfiles nil)
  (setq redisplay-dont-pause t)
  (setq disabled-command-function nil)
  (setq ring-bell-function 'ignore)
  (setq next-screen-context-lines 5)
  (setq read-buffer-completion-ignore-case t)
  (setq indent-tabs-mode nil)
  (setq load-prefer-newer t)
  (setq highlight-nonselected-windows nil)
  (setq kill-buffer-query-functions nil)
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
  (setq-default cache-long-scans t)
  (setq-default word-wrap nil)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default fill-column 80)
  (setq-default line-spacing 0)
  (setq-default truncate-lines t)
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
  ;; Shamelessly lifted from @zarkone's config, and tweaked
  (defun j0ni/delete-whitespace (&optional backward-only)
    "Replaces all spaces, tabs and newlinesaround point with a single space.
If BACKWARD-ONLY is non-nil, only delete them before point."
    (interactive "*P")
    (let ((orig-pos (point)))
      (delete-region
       (if backward-only
           orig-pos
         (progn
           (skip-chars-forward " \t\n")
           (constrain-to-field nil orig-pos t)))
       (progn
         (skip-chars-backward " \t\n")
         (constrain-to-field nil orig-pos)))
      (unless backward-only (insert " "))))
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
  :config
  (when window-system
    (fringe-mode 8)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (tooltip-mode -1))
  (electric-indent-mode t)
  (show-paren-mode 1)
  (save-place-mode 1)
  (global-hl-line-mode 1)
  (column-number-mode 1)
  (winner-mode 1)
  (global-auto-revert-mode 1)
  (blink-cursor-mode -1)
  (display-time-mode 1)
  (remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
  :bind (("M-[" . beginning-of-buffer)
         ("M-]" . end-of-buffer)
         ("C-x C-b" . ibuffer)
         ("C-x C-r" . revert-buffer)
         ("C-x |" . j0ni/toggle-window-split)
         ("C-c ." . j0ni/delete-whitespace)
         ("C-c s" . j0ni/insert-shrug)
         ("C-=" . text-scale-increase)
         ("C--" . text-scale-decrease)))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package diminish)

(use-package modus-themes
  :init
  (setq modus-themes-mode-line nil)
  (setq modus-themes-bold-constructs nil)
  (setq modus-themes-syntax nil)
  (setq modus-themes-fringes nil)
  (setq modus-themes-completions nil) ;; or 'moderate, or 'opinionated
  (setq modus-themes-scale-headings t)
  :config
  (load-theme 'modus-vivendi t)
  (set-face-attribute 'bold nil :weight 'semibold))

(use-package ibuffer-vc
  :hook ((ibuffer-hook . j0ni/ibuffer-vc-hook))
  :custom
  (ibuffer-formats
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
  :init
  (defun j0ni/ibuffer-vc-hook ()
    (ibuffer-auto-mode 1)
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))))

(use-package rainbow-mode)
(use-package rainbow-delimiters)

(use-package browse-at-remote)

(use-package diff-hl
  :diminish
  :hook ((after-init . global-diff-hl-mode)
         (diff-hl-mode . diff-hl-flydiff-mode))
  :config
  (eval-after-load 'magit
    '(progn
       (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
       (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))))

(use-package expand-region
  :bind (("C-x x" . er/expand-region)))

(use-package company
  :diminish
  :custom
  (company-global-modes '(not org-mode telega-chat-mode))
  :hook (after-init . global-company-mode)
  :config
  (push 'company-elisp company-backends))

(use-package dockerfile-mode)

(use-package prescient
  :commands (prescient-persist-mode)
  :hook (after-init . prescient-persist-mode))

(use-package company-prescient
  :diminish
  :commands (company-prescient-mode)
  :hook (company-mode . company-prescient-mode))

(use-package ivy-prescient
  :diminish
  :after (ivy)
  :hook (after-init . ivy-prescient-mode))

(use-package ivy
  :diminish
  :hook (afer-init . ivy-mode)
  :custom
  (ivy-height 15)
  (ivy-wrap t)
  (ivy-use-virtual-buffers t)
  (ivy-extra-directories nil)
  (confirm-nonexistent-file-or-buffer t)
  :init
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c C-r" . ivy-resume)))

(use-package hydra
  :commands (hydra-add-imenu)
  :hook (emacs-lisp-mode . hydra-add-imenu))

(use-package ivy-hydra
  :commands (hydra-ivy/body))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("C-c u" . swiper-all)))

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ;; ("M-y" . counsel-yank-pop)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("<f2> j" . counsel-set-variable)
         ("C-c C" . counsel-compile)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c L" . counsel-git-log)
         ("C-c k" . counsel-rg)
         ;; ("C-c m" 'counsel-linux-app)
         ("C-c n" . counsel-fzf)
         ("C-x l" . counsel-locate)
         ("C-c J" . counsel-file-jump)
         ;; ("C-S-o" . counsel-rhythmbox)
         ("C-c w" . counsel-wmctrl)
         ("C-c b" . counsel-bookmark)
         ("C-c d" . counsel-descbinds)
         ("C-c o" . counsel-outline)
         ("C-c t" . counsel-load-theme)
         ("C-c F" . counsel-org-file)))

(use-package ivy-rich
  :diminish
  :after (ivy)
  :hook (after-init . ivy-rich-mode)
  :custom
  (ivy-rich-display-transformers-list
   '(ivy-switch-buffer
     (:columns
      ((ivy-rich-candidate (:width 40))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 30 :face warning))
       (ivy-rich-switch-buffer-project (:width 20 :face success))
       (ivy-rich-switch-buffer-path
        (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path
                             x (ivy-rich-minibuffer-width 0.4))))))
      :predicate
      (lambda (cand) (get-buffer cand)))
     counsel-find-file
     (:columns
      ((ivy-read-file-transformer)
       (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
     counsel-M-x
     (:columns
      ((counsel-M-x-transformer (:width 45))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-function
     (:columns
      ((counsel-describe-function-transformer (:width 40))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-variable
     (:columns
      ((counsel-describe-variable-transformer (:width 40))
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
     counsel-recentf
     (:columns
      ((ivy-rich-candidate (:width 0.8))
       (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
     package-install
     (:columns
      ((ivy-rich-candidate (:width 40))
       (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
       (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
       (ivy-rich-package-install-summary (:face font-lock-doc-face))))))
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel-projectile
  :diminish
  :commands (counsel-projectile-mode)
  :hook (after-init . counsel-projectile-mode))

(use-package browse-kill-ring
  :init
  (browse-kill-ring-default-keybindings))

(use-package magit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-diff-refine-hunk t)
  (magit-bury-buffer-function 'magit-mode-quit-window)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package browse-at-remote)

(use-package projectile
  :diminish
  :after (ivy)
  :custom
  (projectile-completion-system 'ivy)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package ripgrep
  :config
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

(use-package projectile-ripgrep)

;; Useful knowledge, might deserve some extra binds

;; C-M-n forward-list Move forward over a parenthetical group

;; C-M-p backward-list Move backward over a parenthetical group

;; C-M-f forward-sexp Move forward over a balanced expression

;; C-M-b backward-sexp Move backward over a balanced expression

;; C-M-k kill-sexp Kill balanced expression forward

;; C-M-SPC mark-sexp Put the mark at the end of the sexp.

(use-package paredit
  :diminish " ()"
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode))
  :commands (enable-paredit-mode))

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config))

(use-package which-key
  :diminish
  :hook (after-init . which-key-mode))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?j ?k ?l ?n ?m))
  :commands (ace-window)
  :bind ("C-x o" . ace-window))

(use-package yaml-mode)

(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-js-indent-offset 2)
  (web-mode-script-padding 0)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  :init
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
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

(use-package restclient)

(use-package lsp-mode
  :commands (lsp lsp-register-custom-settings lsp-deferred)
  :custom
  (lsp-enable-indentation nil)
  (lsp-auto-configure t)
  (lsp-enable-xref t)
  (lsp-enable-snippet nil)
  (lsp-auto-guess-root t)
  (lsp-eldoc-render-all t)
  (lsp-signature-render-all t)
  (lsp-keymap-prefix "s-p")
  (lsp-prefer-flymake nil)
  :bind (("C-c d" . lsp-describe-thing-at-point)
         ("C-c e n" . flymake-goto-next-error)
         ("C-c e p" . flymake-goto-prev-error)
         ("C-c e r" . lsp-find-references)
         ("C-c e R" . lsp-rename)
         ("C-c e i" . lsp-find-implementation)
         ("C-c e t" . lsp-find-type-definition)))

(use-package lsp-ivy)

(use-package cider
  :commands (cider-mode)
  :hook ((cider-mode . turn-on-eldoc-mode)
         (cider-repl-mode . enable-paredit-mode))
  :init
  (defun j0ni/cider-modeline-info ()
    "Return info for the cider mode modeline.
Info contains the connection type, project name and host:port endpoint."
    (if-let* ((current-connection (ignore-errors (cider-current-repl))))
        (with-current-buffer current-connection
          (when cider-mode-line-show-connection "✓"))
      "❌"))
  :custom
  (cider-mode-line '(:eval (format " Cider[%s]" (j0ni/cider-modeline-info))))
  (cider-repl-pop-to-buffer-on-connect t)
  (cider-save-file-on-load t)
  (cider-repl-display-help-banner nil)
  (cider-use-overlays t)                ; display eval results inline
  (cider-use-fringe-indicators nil)
  (cider-stacktrace-default-filters '(project))
  (cider-buffer-name-show-port t)
  (cider-repl-history-size 10000)
  (cider-prompt-for-symbol nil)
  (cider-known-endpoints nil)
  (cider-repl-history-file (concat user-emacs-directory ".cider-repl-history"))
  (nrepl-buffer-name-show-port t)
  (cider-prefer-local-resources t)
  (cider-inject-dependencies-at-jack-in t)
  (cider-eldoc-display-context-dependent-info t)
  :config
  (add-to-list 'cider-test-defining-forms "defruns")
  :bind
  (:map cider-repl-mode-map
   ("RET" . cider-repl-newline-and-indent)
   ("C-RET" . cider-repl-return)))

(use-package cider-eval-sexp-fu)

(use-package flycheck-clj-kondo)

(use-package clj-refactor
  :diminish
  :commands (clj-refactor-mode)
  :custom
  (cljr-warn-on-eval nil)
  (cljr-suppress-middleware-warnings t)
  (cljr-favor-prefix-notation nil)
  (cljr-favor-private-functions nil)
  (cljr-inject-dependencies-at-jack-in t)
  (cljr-eagerly-build-asts-on-startup nil)
  (cljr-ignore-analyzer-errors t)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-j"))

(use-package clojure-mode
  :hook ((clojure-mode . cider-mode)
         (clojure-mode . enable-paredit-mode)
         (clojure-mode . flycheck-mode)
         (clojure-mode . clj-refactor-mode))
  :config
  (require 'flycheck-clj-kondo))

(use-package ruby-mode
  :hook (ruby-mode . flycheck-mode))
(use-package inf-ruby)
(use-package rbenv
  :custom ((rbenv-show-active-ruby-in-modeline nil))
  :commands (global-rbenv-mode rbenv-use-corresponding rbenv-use)
  :hook ((after-init . global-rbenv-mode)
         (ruby-mode . rbenv-use-corresponding)))

(use-package json-mode)
(use-package graphql-mode)

(use-package purescript-mode
  :hook (purescript-mode . turn-on-purescript-indentation))

(use-package psc-ide
  :hook (purescript-mode . psc-ide-mode))

(use-package tide
  :commands (tide-setup tide-hl-identifier-mode tide-format-before-save)
  :after (typescript-mode company flycheck)
  :custom
  (typescript-indent-level 2))

(use-package typescript-mode
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . flycheck-mode)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(use-package cargo)
(use-package flycheck-rust)
(use-package rustic
  :hook ((flycheck-mode . flycheck-rust-setup)
         (rustic-mode . lsp)
         (rustic-mode . cargo-minor-mode)
         (rustic-mode . flycheck-mode)
         (rustic-mode . electric-pair-mode))
  :custom
  (indent-tabs-mode nil)
  (rustic-format-trigger 'on-save)
  (rustic-lsp-server 'rust-analyzer)
  ;;(rustic-compile-command "cargo build")
  :config
  (require 'lsp-rust)
  (setq lsp-rust-server 'rust-analyzer)
  (push 'rustic-clippy flycheck-checkers))

(use-package org-plus-contrib
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :custom
  (org-startup-indented t)
  ;; make it short to start with
  (org-startup-folded t)
  ;; where things live
  (org-directory "~/Dropbox/OrgMode/")
  ;; Set agenda file(s)
  (org-agenda-files (list (concat org-directory "journal.org")
                          (concat org-directory "berlin.org")
                          (concat org-directory "shrieks.org")))
  (org-agenda-span 14)
  ;; prevent org-mode hijacking arrow keys
  (org-replace-disputed-keys t)
  ;; set our own todo keywords
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w!)" "PAUSED(p!)" "|" "DONE(d!)" "ABANDONED(a!)")))
  (org-tag-persistent-alist
   '((home . ?h)
     (xapix . ?x)
     (sanity . ?s)
     (rachel . ?r)
     (lauren . ?l)
     (ari . ?a)
     (grace . ?g)
     (family . ?f)
     (self . ?m)))
  ;; switch quickly
  (org-use-fast-todo-selection 'auto)
  (org-priority-default ?C)
  ;; extra indentation
  (org-adapt-indentation t)
  ;; Let's have pretty source code blocks
  (org-edit-src-content-indentation 0)
  (org-src-tab-acts-natively t)
  (org-src-fontify-natively t)
  (org-confirm-babel-evaluate nil)
  (org-default-notes-file (concat org-directory "/berlin.org"))
  (org-capture-templates
   `(("j" "Journal" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
      "* %T\n  %?\n\n%a")
     ("s" "Shriek" entry (file+headline ,(concat org-directory "/shrieks.org") "Shrieks")
      "* %T\n%?\n")
     ("t" "Task" entry (file+headline ,(concat org-directory "/berlin.org") "Inbox")
      "* TODO %?\n  %a\n%i")
     ("b" "BP Journal" entry (file+olp+datetree ,(concat org-directory "/bp.org") "Blood Pressure")
      "* %T\n** Systolic: %^{systolic}\n** Diastolic: %^{diastolic}\n** Pulse: %^{pulse}\n** Notes\n%?\n")))
  :init
  (defun j0ni/org-mode-hook ()
    (visual-line-mode 1)
    (add-hook 'before-save-hook 'org-update-all-dblocks nil 'local-only))
  :hook ((org-mode . j0ni/org-mode-hook)
         (org-capture-mode . j0ni/org-mode-hook))
  :config
  ;; org-capture - for inserting into date based trees
  (require 'org-datetree)
  ;; needed for structure templates (<s-TAB etc)
  (require 'org-tempo)
  (org-clock-persistence-insinuate))

(use-package org-roam
  :diminish
  :hook ((after-init . org-roam-mode))
  :custom
  (org-roam-directory (expand-file-name "org-roam" org-directory))
  (org-roam-completion-system 'ivy)
  (org-roam-buffer-position 'bottom)
  :init
  (defun j0ni/roam-todo ()
    "An ad-hoc agenda for `org-roam'. Shamelessly stolen from abo-abo."
    (interactive)
    (let* ((regex "^\\* TODO")
           (b (get-buffer (concat "*ivy-occur counsel-rg \"" regex "\"*"))))
      (if b
          (progn
            (switch-to-buffer b)
            (ivy-occur-revert-buffer))
        (setq unread-command-events (listify-key-sequence (kbd "C-c C-o M->")))
        (counsel-rg regex org-roam-directory "--sort modified"))))
  (defhydra hydra-org-roam (:exit t :idle 0.8)
    "Launcher for `org-roam'."
    ("i" org-roam-insert "insert")
    ("f" org-roam-find-file "find-file")
    ("v" org-roam-buffer-activate "backlinks")
    ("t" j0ni/roam-todo "todo"))
  :bind
  (("<f5>" . hydra-org-roam/body)))

(use-package telega
  :commands (telega telega-mode-line-mode)
  :bind (("C-x M-t" . telega))
  :hook (telega-chat-mode . visual-line-mode)
  :config
  (telega-mode-line-mode t))

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package all-the-icons)

(use-package lpy
  :commands (lpy-mode)
  :hook (python-mode . lpy-mode))

(use-package haskell-mode
  :hook ((haskell-mode . electric-pair-mode)))

(use-package olivetti
  :custom
  ((olivetti-body-width 120)))

;; mu4e isn't packaged in the usual way, it gets installed as part of the `mu` system package.

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(defun j0ni/mu4e-bookmark (sub-maildir days char)
  (list (concat "date:" days "d..now AND (maildir:/" sub-maildir "/INBOX OR maildir:/" sub-maildir "/sent-mail) AND NOT flag:trashed")
        (concat "Last " days " days (" sub-maildir ")")
        char))

(setq mu4e-decryption-policy t
      mu4e-update-interval 300
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
      mu4e-split-view 'single-window
      mu4e-headers-fields '((:human-date . 12)
                            (:flags . 6)
                            (:mailing-list . 16)
                            (:from-or-to . 25)
                            (:thread-subject))
      mu4e-compose-complete-only-after "2012-01-01"
      mu4e-view-show-addresses t
      mm-inline-large-images 'resize
      message-send-mail-function 'smtpmail-send-it
      message-kill-buffer-on-exit t
      mail-user-agent 'mu4e-user-agent
      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format "On %a, %d %b %Y at %T %z, %f wrote:"
      mu4e-personal-addresses '("j@lollyshouse.ca"
                                "hi@mhcat.ca"
                                "jonathan.irving@gmail.com"
                                "jon@xapix.io"
                                "j0ni@fastmail.com"
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
                     (mu4e-compose-signature . "https://j0ni.ca ~ https://keybase.io/j0ni")
                     (mu4e-bookmarks . ,(list (j0ni/mu4e-bookmark "Fastmail" "7" ?w)
                                              (j0ni/mu4e-bookmark "Fastmail" "30" ?m)))
                     (smtpmail-smtp-user . "j0ni@fastmail.com")
                     (smtpmail-smtp-server . "smtp.fastmail.com")
                     (smtpmail-smtp-service . 587)
                     (smtpmail-stream-type . starttls)))
            (make-mu4e-context
             :name "Xapix"
             :enter-func (lambda ()
                           (when (mu4e-running-p)
                             (mu4e-update-mail-and-index nil))
                           (mu4e-message "Switching to Xapix context"))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/Xapix" (mu4e-message-field msg :maildir))))
             :vars `((user-mail-address . "jon@xapix.io")
                     (user-full-name . "Jon Irving")
                     (mu4e-sent-messages-behavior . sent)
                     (mu4e-sent-folder . "/Xapix/sent-mail")
                     (mu4e-trash-folder . "/Xapix/trash")
                     (mu4e-drafts-folder . "/Xapix/drafts")
                     (mu4e-refile-folder . "/Xapix/all-mail")
                     (mu4e-maildir-shortcuts . (("/Xapix/INBOX" . ?i)
                                                ("/Xapix/sent-mail" . ?s)
                                                ("/Xapix/drafts" . ?d)
                                                ("/Xapix/trash" . ?t)))
                     (mu4e-compose-signature . "https://j0ni.ca ~ https://keybase.io/j0ni ~ https://xapix.io")
                     (mu4e-bookmarks . ,(list (j0ni/mu4e-bookmark "Xapix" "7" ?w)
                                              (j0ni/mu4e-bookmark "Xapix" "30" ?m)))
                     (smtpmail-smtp-user . "jon@xapix.io")
                     (smtpmail-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-service . 587)
                     (smtpmail-stream-type . starttls)))))

(add-hook 'message-mode-hook #'turn-on-auto-fill)
(add-hook 'message-mode-hook #'mml-secure-message-sign-pgpmime)

;; Do this last, since it may contain references to package functions
(require 'keys)
