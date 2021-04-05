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

;; (debug-watch 'indent-tabs-mode)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'boot)

(use-package emacs
  :hook ((before-save-hook . delete-trailing-whitespace)
         (prog-mode-hook . whitespace-mode)
         (whitespace-mode-hook . (lambda () (diminish 'whitespace-mode)))
         (after-init-hook . recentf-mode)
         (after-init-hook . savehist-mode))
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
  (compilation-always-kill t)
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
  (shr-color-visible-luminance-min 90)
  :init
  (setq-default browse-url-browser-function
                (cl-case system-type
                  ((darwin macos) 'browse-url-default-macosx-browser)
                  (t 'browse-url-firefox)))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  ;;(set-default-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-face-font 'variable-pitch "Lucida Grande-10" nil)
  (set-frame-font "Iosevka Snuggle Light-15" t t)
  (set-face-font 'fixed-pitch "Iosevka Snuggle Light-15" nil)
  (set-face-font 'fixed-pitch-serif "Iosevka Snuggle Light-15" nil)
  ;; (set-frame-font "PragmataPro Liga-11" t t)
  ;; (set-face-font 'fixed-pitch "PragmataPro Liga-11" nil)
  ;; (set-face-font 'fixed-pitch-serif "PragmataPro Liga-11" nil)
  (set-frame-font "Monoid-10" t t)
  (set-face-font 'fixed-pitch "Monoid-10" nil)
  (set-face-font 'fixed-pitch-serif "Monoid-10" nil)
  (set-fontset-font t 'unicode "Symbola" nil 'prepend)
  (when (string= system-type "gnu/linux")
    (setq x-super-keysym 'meta))
  (setq scroll-step 0)
  (setq scroll-margin 2)
  (setq auto-window-vscroll nil)
  ;; be sure to set this to 0 in any auto-scrolling buffers
  (setq scroll-conservatively 100000)
  (setq scroll-preserve-screen-position t)
  (setq gc-cons-threshold (* 50 1024 1024))
  (setq auto-revert-verbose nil)
  (setq create-lockfiles nil)
  (setq redisplay-dont-pause t)
  (setq disabled-command-function nil)
  (setq ring-bell-function 'ignore)
  (setq next-screen-context-lines 5)
  (setq read-buffer-completion-ignore-case t)
  (setq-default indent-tabs-mode nil)
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
  (setq-default line-spacing 1)
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
  (defun j0ni/read-string-from-file (file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))
  :config
  (when window-system
    (fringe-mode 8)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (tooltip-mode -1))
  (electric-indent-mode 1)
  (show-paren-mode 1)
  (save-place-mode 1)
  (global-hl-line-mode 1)
  (column-number-mode 1)
  (winner-mode 1)
  (global-auto-revert-mode 1)
  (blink-cursor-mode -1)
  (remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
  (diminish 'eldoc-mode)
  (advice-add #'shr-colorize-region :around (defun shr-no-colorise-region (&rest ignore)))
  :bind (("M-[" . beginning-of-buffer)
         ("M-]" . end-of-buffer)
         ("C-x C-r" . revert-buffer)
         ("C-c C-k" . eval-buffer)
         ("C-x |" . j0ni/toggle-window-split)
         ("C-c ." . j0ni/delete-whitespace)
         ("C-c s" . j0ni/insert-shrug)
         ("C-=" . text-scale-increase)
         ("C--" . text-scale-decrease)))

(use-package ligature
  :commands (ligature-set-ligatures)
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :config
  ;; use the non-annoying subset of the list found here:
  ;; https://github.com/fabrizioschiavi/pragmatapro/blob/master/emacs_snippets/pragmatapro-prettify-symbols-v0.828.el
  (ligature-set-ligatures 'prog-mode
                          '("!!" "!=" "!==" "!!!" "!≡" "!≡≡" "!>" "!=<" "#(" "#_" "#{"
                            "#?" "#>" "##" "#_(" "%=" "%>" "%>%" "%<%" "&%" "&&" "&*"
                            "&+" "&-" "&/" "&=" "&&&" "&>" "$>" "***" "*=" "*/" "*>"
                            "++" "+++" "+=" "+>" "++=" "--" "-<" "-<<" "-=" "->" "->>"
                            "---" "-->" "-+-" "-\\/" "-|>" "-<|" ".." "..." "..<" ".>"
                            ".~" ".=" "/*" "//" "/>" "/=" "/==" "///" "/**" ":::" "::"
                            ":=" ":≡" ":>" ":=>" ":(" ":-(" ":)" ":-)" ":/" ":\\" ":3"
                            ":D" ":P" ":>:" ":<:" "<$>" "<*" "<*>" "<+>" "<-" "<<" "<<<"
                            "<<=" "<=" "<=>" "<>" "<|>" "<<-" "<|" "<=<" "<~" "<~~" "<<~"
                            "<$" "<+" "<!>" "<@>" "<#>" "<%>" "<^>" "<&>" "<?>" "<.>"
                            "</>" "<\\>" "<\">" "<:>" "<~>" "<**>" "<<^" "<!" "<@" "<#"
                            "<%" "<^" "<&" "<?" "<." "</" "<\\" "<\"" "<:" "<->" "<!--"
                            "<--" "<~<" "<==>" "<|-" "<<|" "<-<" "<-->" "<<==" "<=="
                            "=<<" "==" "===" "==>" "=>" "=~" "=>>" "=/=" "=~=" "==>>"
                            "≡≡" "≡≡≡" "≡:≡" ">-" ">=" ">>" ">>-" ">>=" ">>>" ">=>"
                            ">>^" ">>|" ">!=" ">->" "??" "?~" "?=" "?>" "???" "?." "^="
                            "^." "^?" "^.." "^<<" "^>>" "^>" "\\\\" "\\>" "\\/-" "@>"
                            "|=" "||" "|>" "|||" "|+|" "|->" "|-->" "|=>" "|==>" "|>-"
                            "|<<" "||>" "|>>" "|-" "||-" "~=" "~>" "~~>" "~>>" "[[" "]]"
                            "\">" "_|_"))
  ;; Iosevka
  ;; (ligature-set-ligatures 'prog-mode
  ;;                         '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->"
  ;;                           "<--->" "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>" "==>"
  ;;                           "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---" "<~~" "<~"
  ;;                           "~>" "~~>" "::" ":::" "==" "!=" "===" "!==" ":=" ":-" ":+"
  ;;                           "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>"
  ;;                           "++" "+++"))


  :hook ((prog-mode-hook . ligature-mode)))


(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-use-other-window nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-movement-cycle nil)
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-use-header-line t)
  (ibuffer-default-shrink-to-minimum-size nil)
  (ibuffer-saved-filter-groups nil)
  (ibuffer-old-time 48))

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

(use-package find-file-in-project
  :commands (find-file-in-project-by-selected
             ffip-get-project-root-directory)
  :bind (("C-c f" . find-file-in-project-by-selected))
  :init
  (setq ffip-use-rust-fd t))

(use-package circe)

(use-package undo-fu
  :custom
  (undo-fu-allow-undo-in-region t)
  :bind (("C-_" . undo-fu-only-undo)
         ("C-/" . undo-fu-only-undo)
         ("C-z" . undo-fu-only-undo)
         ("<undo>" . undo-fu-only-undo)
         ("C-x u" . undo-fu-only-undo)
         ("M-_" . undo-fu-only-redo)
         ("C-M-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :hook ((after-init-hook . global-undo-fu-session-mode)))

(use-package exec-path-from-shell
  :init
  (defvar j0ni/exec-path-from-shell-completed nil "Stop this happening repeatedly")
  (when (and j0ni/exec-path-from-shell-completed (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)
    (setq j0ni/exec-path-from-shell-completed t)))

(use-package modus-themes
  :hook ((after-init-hook . modus-themes-load-themes))
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-slanted-constructs t)
  (modus-themes-syntax nil) ;; 'faint
  (modus-themes-fringes 'subtle)
  (modus-themes-completions 'opinionated)
  (modus-themes-scale-headings t)
  (modus-themes-mode-line nil)
  (modus-themes-paren-match 'intense-bold)
  :config
  ;; (load-theme 'modus-operandi t)
  (load-theme 'modus-vivendi t)
  (set-face-attribute 'bold nil :weight 'semibold))

(use-package hl-todo
  :hook ((after-init-hook . global-hl-todo-mode)))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1)
  (diminish 'volatile-highlights-mode))

(use-package gruvbox-theme)
(use-package cyberpunk-theme
  :init
  ;; (load-theme 'cyberpunk t)
  ;; (set-face-attribute 'default nil :background "#000000")
  )
(use-package zerodark-theme)
(use-package doom-themes
  ;; :init
  ;; (load-theme 'doom-old-hope t)
  ;; (set-face-attribute 'default nil :background "#000000")
  )

(use-package zenburn-theme)
(use-package dracula-theme
  ;; :init
  ;; (load-theme 'dracula t)
  ;; (set-face-attribute 'bold nil :weight 'semibold)
  ;; (set-face-attribute 'default nil :background "#000000")
  )

(use-package almost-mono-themes
  ;; :hook ((after-init-hook . (lambda () (load-theme 'almost-mono-black t))))
  ;; :init
  ;; (load-theme 'almost-mono-black t)
  ;; (set-face-attribute 'bold nil :weight 'semibold)
  )

(use-package rainbow-mode
  :bind (("C-c r" . rainbow-mode)))

(use-package rainbow-delimiters
  ;; :hook ((paredit-mode-hook . rainbow-delimiters-mode))
  )

(use-package browse-at-remote)

(use-package diff-hl
  :diminish ""
  :hook ((after-init-hook . global-diff-hl-mode))
  :config
  (eval-after-load 'magit
    '(progn
       (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
       (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

(use-package expand-region
  :bind (("C-x C-x" . er/expand-region)))

(use-package company
  :diminish ""
  :custom
  (company-global-modes '(not org-mode))
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 2)
  (company-idle-delay 1.0)
  (company-tooltip-idle-delay 1.0)
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-if-just-one-frontend
                       company-echo-metadata-frontend))
  :hook ((after-init-hook . global-company-mode))
  :bind (("M-\\" . company-complete)
         ("C-\\" . company-complete)
         :map company-active-map
         ("M-\\" . company-complete-common-or-cycle)
         ("C-\\" . company-complete-common-or-cycle)
         ("C-j" . company-complete-selection)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (push 'company-elisp company-backends))

(use-package dockerfile-mode)

(use-package anzu
  :hook ((after-init-mode . anzu-mode)))

(use-package prescient
  :commands (prescient-persist-mode)
  :hook ((after-init-hook . prescient-persist-mode)))

(use-package company-prescient
  :diminish ""
  :commands (company-prescient-mode)
  :hook ((company-mode-hook . company-prescient-mode)))

(use-package selectrum
  ;; :hook ((after-init-hook . selectrum-mode))
  :bind (:map selectrum-minibuffer-map
              ("C-j" . selectrum-select-current-candidate))
  :custom
  (selectrum-max-window-height 15))

(use-package orderless)

(use-package emacs
  :custom
  (completion-styles '(orderless partial-completion))
  (completion-cycle-threshold 3)
  (completion-flex-nospace nil)
  (completion-pcm-complete-word-inserts-delimiters t)
  (completion-pcm-word-delimiters "-_./:| ")
  (completion-show-help nil)
  (completion-auto-help nil)
  ;; The following two are updated in Emacs 28.  They concern the
  ;; *Completions* buffer.  Note that I actually do not use that buffer,
  ;; because I rely on Embark's version of it.
  (completions-format 'one-column)
  (completions-detailed t)
  (read-file-name-completion-ignore-case t)
  (read-answer-short t)
  :init
  (setq completion-category-defaults nil)
  (setq completion-ignore-case t)
  (setq-default case-fold-search t)     ; For general regexp
  (setq read-buffer-completion-ignore-case t)
  (setq enable-recursive-minibuffers t)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)
  (setq echo-keystrokes 0.25)           ; from the C source code

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

(use-package selectrum-prescient
  ;; :hook ((after-init-hook . selectrum-prescient-mode))
  )

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-s" . consult-line)
   ("C-r" . consult-isearch)
   ("C-c i" . consult-imenu))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package marginalia
  :hook ((after-init-hook . marginalia-mode))
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package embark
  :custom
  (embark-collect-initial-view-alist
   '((file . list)
     (buffer . list)
     (symbol . list)
     (line . list)
     (xref-location . list)
     (kill-ring . zebra)
     (t . list)))
  (embark-quit-after-action t)          ; XXX: Read the doc string!
  (embark-collect-live-update-delay 0.5)
  (embark-collect-live-initial-delay 1.5)
  :init
  (defun j0ni/switch-to-minibuffer ()
    "Switch to minibuffer window."
    (interactive)
    (if (active-minibuffer-window)
        (select-window (active-minibuffer-window))
      (error "Minibuffer is not active")))
  :hook ((minibuffer-setup-hook . embark-collect-completions-after-input)
         (embark-post-action-hook . embark-collect--update-linked))
  :bind (("C-," . embark-act)
         :map minibuffer-local-completion-map
         ("C-," . embark-act)
         ("C->" . embark-become)
         ("M-q" . embark-collect-toggle-view)
         ("M-v" . embark-switch-to-collect-completions)
         :map embark-collect-mode-map
         ("C-," . embark-act)
         ("," . embark-act)
         ("M-q" . embark-collect-toggle-view)
         ("C-c q" . j0ni/switch-to-minibuffer)
         :map embark-region-map
         ("i" . epa-import-keys-region)
         ("s" . sort-lines)
         :map embark-symbol-map
         ("." . embark-find-definition)
         ("k" . describe-keymap)))

(use-package embark-consult
  :hook ((embark-collect-mode-hook . embark-consult-preview-minor-mode)))

(use-package browse-kill-ring
  :init
  (browse-kill-ring-default-keybindings))

(use-package magit
  :custom
  ;;(magit-completing-read-function #'ivy-completing-read)
  ;;(magit-completing-read-function #'selectrum-completing-read)
  (magit-diff-refine-hunk t)
  (magit-bury-buffer-function #'magit-mode-quit-window)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package browse-at-remote)

(use-package highlight-symbol
  :hook ((prog-mode-hook . highlight-symbol-mode)))

(use-package projectile
  :hook ((after-init-hook . projectile-mode))
  :diminish ""
  :custom
  (projectile-sort-order 'recently-active)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package ripgrep
  :config
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

;; (use-package projectile-ripgrep)

;; Useful knowledge, might deserve some extra binds

;; C-M-n forward-list Move forward over a parenthetical group

;; C-M-p backward-list Move backward over a parenthetical group

;; C-M-f forward-sexp Move forward over a balanced expression

;; C-M-b backward-sexp Move backward over a balanced expression

;; C-M-k kill-sexp Kill balanced expression forward

;; C-M-SPC mark-sexp Put the mark at the end of the sexp.

(use-package paredit
  :diminish ""
  :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
         (lisp-mode-hook . enable-paredit-mode)
         (scheme-mode-hook . enable-paredit-mode))
  :commands (enable-paredit-mode))

(use-package geiser
  :commands (turn-on-geiser-mode)
  :hook ((scheme-mode-hook . turn-on-geiser-mode)
         (racket-mode-hook . turn-on-geiser-mode)
         (geiser-repl-mode-hook . enable-paredit-mode)))

(use-package racket-mode
  :hook ((racket-mode-hook . enable-paredit-mode)))

(use-package smartparens
  :diminish ""
  ;; :config
  ;; (require 'smartparens-config)
  )

(use-package which-key
  :diminish ""
  :hook (after-init-hook . which-key-mode))

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
  :hook ((clojure-mode-hook . lsp)
         (clojurescript-mode-hook . lsp)
         (clojurec-mode-hook . lsp))
  :commands (lsp lsp-register-custom-settings lsp-deferred)
  :config
  (setq cljr-add-ns-to-blank-clj-files nil)
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  :custom
  (lsp-enable-indentation t)
  (lsp-enable-completion-at-point t)
  (lsp-auto-configure t)
  (lsp-enable-xref t)
  (lsp-enable-snippet nil)
  (lsp-auto-guess-root nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-render-all nil)
  (lsp-signature-render-all t)
  (lsp-enable-symbol-highlighting t)
  (lsp-idle-delay 0.8)
  (lsp-lens-enable t)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold 10000)
  (lsp-signature-auto-activate nil)
  (lsp-completion-provider :capf)
  :bind (:map lsp-mode-map
              ("C-c C-l d" . lsp-describe-thing-at-point)
              ("C-c C-l r" . lsp-rename)
              ("C-c C-l i" . lsp-find-implementation)
              ("C-c C-l ." . lsp-find-type-definition)))

(use-package lsp-ui
  :config
  (require 'lsp-ui-imenu)
  :custom
  (lsp-ui-autoconfigure t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show nil)
  (lsp-ui-imenu-autorefresh t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  :bind
  (:map lsp-ui-mode-map
        ("C-c l I" . lsp-ui-imenu)
        ("C-c l ." . lsp-ui-peek-find-definitions)
        ("C-c l ?" . lsp-ui-peek-find-references)
        ("C-c l r" . lsp-rename)
        ("C-c l x" . lsp-workspace-restart)
        ("C-c l w" . lsp-ui-peek-find-workspace-symbol)
        ("C-c l i" . lsp-ui-peek-find-implementation)
        ("C-c l d" . lsp-describe-thing-at-point)
        ("C-c l e" . lsp-execute-code-action)))

(use-package sly
  :hook ((sly-mrepl-hook . company-mode))
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package sly-quicklisp)
(use-package sly-macrostep)
(use-package sly-asdf)

;; (use-package inf-clojure
;;   :hook ((clojure-mode-hook . inf-clojure-minor-mode)
;;          (inf-clojure-mode-hook . eldoc-mode)
;;          (inf-clojure-mode-hook . enable-paredit-mode)))

(use-package cider
  :commands (cider-mode)
  :hook ((cider-mode-hook . turn-on-eldoc-mode)
         (cider-repl-mode-hook . enable-paredit-mode))
  :init
  (defun j0ni/cider-modeline-info ()
    "Return info for the cider mode modeline.
Info contains the connection type, project name and host:port endpoint."
    (if-let* ((current-connection (ignore-errors (cider-current-repl))))
        (with-current-buffer current-connection
          (when cider-mode-line-show-connection "✓"))
      "❌"))
  :custom
  (cider-mode-line '(:eval (format " CIDER[%s]" (j0ni/cider-modeline-info))))
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

;; (use-package flycheck-clj-kondo)

(use-package clj-refactor
  :diminish ""
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
  ;; :config
  ;; (require 'flycheck-clj-kondo)
  :hook ((clojure-mode-hook . enable-paredit-mode)
         (clojure-mode-hook . subword-mode)
         (clojure-mode-hook . cider-mode)
         ;; (clojure-mode-hook . flycheck-mode)
         (clojure-mode-hook . clj-refactor-mode)))

(use-package ruby-mode
  :hook (ruby-mode-hook . flycheck-mode))
(use-package inf-ruby)
(use-package rbenv
  :custom
  (rbenv-show-active-ruby-in-modeline nil)
  :commands (global-rbenv-mode rbenv-use-corresponding rbenv-use)
  :hook ((after-init-hook . global-rbenv-mode)
         (ruby-mode-hook . rbenv-use-corresponding)))

(use-package json-mode)
(use-package graphql-mode)

(use-package purescript-mode
  :hook ((purescript-mode-hook . turn-on-purescript-indentation)))

(use-package psc-ide
  :hook ((purescript-mode-hook . psc-ide-mode)))

(use-package typescript-mode
  :hook ((typescript-mode-hook . lsp-mode)))

(use-package eros
  :hook ((after-init-hook . eros-mode)))

(use-package flycheck
  :hook ((prog-mode-hook . flycheck-mode))
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(use-package rustic
  :commands (rustic-mode)
  :hook ((rustic-mode-hook . lsp-mode)
         (rustic-mode-hook . electric-pair-mode))
  :custom
  (rustic-format-trigger 'on-save)
  (rustic-indent-method-chain t)
  (rustic-lsp-server 'rust-analyzer)
  (rustic-lsp-format t)
  (rustic-lsp-client 'lsp)
  (rustic-indent-method-chain nil)
  :config
  (setq indent-tabs-mode nil)
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
  (org-log-done 'note)
  (org-log-into-drawer t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-use-speed-commands t)
  (org-clock-persist t)
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
      "* %T\n%?\n\n%a")
     ("s" "Shriek" entry (file+headline ,(concat org-directory "/shrieks.org") "Shrieks")
      "* %T\n%?\n")
     ("t" "Task" entry (file+headline ,(concat org-directory "/berlin.org") "Inbox")
      "* TODO %?\n  %a\n%i")
     ("b" "BP Journal" entry (file+olp+datetree ,(concat org-directory "/bp.org") "Blood Pressure")
      "* %T\n** Systolic: %^{systolic}\n** Diastolic: %^{diastolic}\n** Pulse: %^{pulse}\n** Notes\n%?\n")))
  :init
  (defun j0ni/org-mode-hook ()
    ;; org-capture - for inserting into date based trees
    (require 'org-datetree)
    ;; needed for structure templates (<s-TAB etc)
    (require 'org-tempo)
    (org-clock-persistence-insinuate)
    (visual-line-mode 1)
    (add-hook 'before-save-hook 'org-update-all-dblocks nil 'local-only))
  :hook ((org-mode-hook . j0ni/org-mode-hook)
         (org-capture-mode-hook . j0ni/org-mode-hook)))

(use-package org-super-agenda
  :after (org-with-contrib)
  :custom
  (org-super-agenda-groups '((:auto-dir-name t)))
  :hook ((org-agenda-mode-hook . org-super-agenda-mode)))

(use-package org-roam
  :diminish ""
  :hook ((after-init-hook . org-roam-mode))
  :custom
  (org-roam-directory (expand-file-name "org-roam" org-directory))
  (org-roam-completion-system 'ivy)
  (org-roam-buffer-position 'bottom)
  ;; :init
  ;; (defhydra hydra-org-roam (:exit t :idle 0.8)
  ;;   "Launcher for `org-roam'."
  ;;   ("c" org-roam-capture "capture")
  ;;   ("i" org-roam-insert "insert")
  ;;   ("f" org-roam-find-file "find-file")
  ;;   ("v" org-roam-buffer-activate "backlinks"))
  ;; :bind
  ;; (("<f5>" . hydra-org-roam/body)
  ;;  ("C-c C" . org-roam-capture))
  )

(use-package elfeed
  :custom
  (elfeed-feeds '("https://pluralistic.net/feed/")))

(use-package telega
  :commands (telega
             telega-mode-line-mode
             telega-notifications-mode)
  :bind (("C-x M-t" . telega))
  :hook ((telega-chat-mode-hook . visual-line-mode))
  :config
  (telega-mode-line-mode t)
  (telega-notifications-mode t)
  (eval-after-load 'modus-themes
    '(defadvice modus-themes-toggle (after clear-telega-icon-cache activate)
       (setq telega-mode-line--logo-image-cache nil))))

(use-package doom-modeline
  :init
  (setq image-scaling-factor 1.4)
  :custom
  (doom-modeline-icon nil)
  :hook ((after-init-hook . doom-modeline-mode)))

(use-package markdown-mode
  :hook ((markdown-mode-hook . visual-line-mode)))

(use-package all-the-icons)

(use-package lpy
  :commands (lpy-mode)
  :hook ((python-mode-hook . lpy-mode)))

(use-package haskell-mode
  :hook ((haskell-mode-hook . electric-pair-mode)))

(use-package olivetti
  :custom
  (olivetti-body-width 120))

;; mu4e isn't packaged in the usual way, it gets installed as part of the `mu` system package.

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(defun j0ni/mu4e-bookmark (sub-maildir days char)
  (list (concat "date:" days "d..now AND (maildir:/" sub-maildir "/INBOX OR maildir:/" sub-maildir "/sent-mail) AND NOT flag:trashed")
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
                     (mu4e-compose-signature . "In this world / we walk on the roof of hell / gazing at flowers\n    - Kobayashi Issa\n\nhttps://j0ni.ca ~ https://keybase.io/j0ni")
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
                     (mu4e-compose-signature . "https://j0ni.ca ~ https://keybase.io/j0ni")
                     (mu4e-bookmarks . ,(list (j0ni/mu4e-bookmark "Well" "7" ?w)
                                              (j0ni/mu4e-bookmark "Well" "30" ?m)))
                     (smtpmail-smtp-user . "joni")
                     (smtpmail-smtp-server . "iris.well.com")
                     (smtpmail-smtp-service . 587)
                     (smtpmail-stream-type . starttls)))))

(add-hook 'message-mode-hook #'turn-on-auto-fill)
(add-hook 'message-mode-hook #'mml-secure-message-sign-pgpmime)

(use-package 2048-game)

;; Do this last, since it may contain references to package functions
(require 'keys)
