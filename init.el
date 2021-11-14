;;; init.el --- emacs config -*- no-byte-compile: t -*-
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

(defvar j0ni/fixed-font nil
  "Should be a string like \"Fira Code Mono-11\" or such.")

(defvar j0ni/variable-font nil
  "Should be a string like \"Fira Code-11\" or such.")

(defvar j0ni/is-mac (memq window-system '(mac ns)))

(defvar j0ni/completion-system 'builtin
  "Should be a symbol, currently 'selectrum, 'ivy, 'icomplete,
'embark, 'vertico, 'mct or 'builtin")

(use-package whitespace-mode
  :init
  (setq whitespace-line-column 100)
  (setq whitespace-style '(face trailing lines-tail tabs))

  :hook
  ((prog-mode-hook . whitespace-mode)
   (whitespace-mode-hook . (lambda () (diminish 'whitespace-mode)))))

(use-package emacs
  :init
  (setq warning-suppress-types '((comp)))
  (setq epa-pinentry-mode 'loopback)
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq inhibit-startup-screen t)
  (setq auto-revert-verbose t)
  (setq vc-follow-symlinks t)
  (setq find-file-suppress-same-file-warnings t)
  (setq read-file-name-completion-ignore-case t)
  (setq comint-prompt-read-only t)
  ;; This defaults to a warning - abo-abo uses advice quite a bit, which is
  ;; where this comes from; I don't see much in the way of warnings yet so let's
  ;; leave this out for now.
  ;; (ad-redefinition-action 'accept)
  ;; hmm
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
  (setq enable-local-variables :all)
  (setq confirm-kill-emacs nil)
  (setq sentence-end-double-space nil)
  (setq delete-old-versions t)
  (setq version-control t)
  (setq custom-safe-themes t)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
  (setq mouse-wheel-progressive-speed t)              ; accelerate scrolling
  (setq shr-color-visible-luminance-min 90)
  ;; 43.67066, -79.30211 - location
  (setq calendar-longitude 43.67066)
  (setq calendar-latitude -79.30211)
  (setq calendar-location-name "Toronto")
  (setq minibuffer-eldef-shorten-default t)
  ;; some fns
  (defun j0ni/init-frame ()
    (when window-system
      (fringe-mode 8)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1)))
  (j0ni/init-frame)
  ;; This is an attempt to prevent the emacsclient frame from ignoring all this
  ;; stuff. Unfortunately it does not appear to work.
  ;; (add-to-list 'after-make-frame-functions #'j0ni/init-frame)
  (set-frame-parameter (selected-frame) 'alpha '(95 . 80))
  ;; OS dependent modifier setup
  (when j0ni/is-mac
    ;; The left and right Option or Alt keys.
    (setq ns-alternate-modifier 'meta)
    (setq ns-right-alternate-modifier 'left)
    ;; The left and right Command keys.
    (setq ns-command-modifier 'meta)
    (setq ns-right-command-modifier 'super)
    ;; The left and right Control keys.
    ;; (setq ns-control-modifier 'control)
    ;; (setq ns-right-control-modifier 'control)
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
  (prefer-coding-system 'utf-8)
  (setq j0ni/completion-system 'mct)
  ;; (setq j0ni/font "Monoisome-10")
  ;; (setq j0ni/fixed-font "OpenDyslexicMono Nerd Font-10")
  ;; (setq j0ni/variable-font "OpenDyslexic Nerd Font-11")
  ;; (setq j0ni/font "PT Mono-11")
  ;; (setq j0ni/font "Monoisome-10")
  ;; (setq j0ni/font "Lucida Grande Mono-11")
  ;; (setq j0ni/font "Lucida Console Patched-11")
  (setq j0ni/fixed-font "Iosevka Snuggle-13")
  ;; (setq j0ni/fixed-font "PragmataPro Mono-12")
  ;; (setq j0ni/fixed-font "Monoid Nerd Font-10")
  ;; (setq j0ni/fixed-font "Monoid-10")
  (setq j0ni/variable-font "Sans-11")
  ;; (setq j0ni/font "Fira Code-13")
  (set-face-font 'variable-pitch j0ni/variable-font nil)
  (set-frame-font j0ni/fixed-font t t)
  (set-face-font 'fixed-pitch j0ni/fixed-font nil)
  (set-face-font 'fixed-pitch-serif j0ni/fixed-font nil)
  (set-fontset-font t 'unicode "Symbola" nil 'prepend)
  (when (string= system-type "gnu/linux")
    (setq x-super-keysym 'meta))
  (setq x-underline-at-descent-line t)
  (setq scroll-step 0)
  (setq scroll-margin 2)
  (setq auto-window-vscroll nil)
  ;; be sure to set this to 0 in any auto-scrolling buffers
  (setq scroll-conservatively 100000)
  (setq scroll-preserve-screen-position t)
  (setq gc-cons-threshold (* 50 1024 1024))
  (setq create-lockfiles nil)
  (setq redisplay-dont-pause t)
  (setq disabled-command-function nil)
  (setq ring-bell-function 'ignore)
  (setq next-screen-context-lines 5)
  (setq read-buffer-completion-ignore-case t)
  (setq-default indent-tabs-mode nil)
  (setq indent-tabs-mode nil)
  (setq tab-always-indent 'complete)
  (setq load-prefer-newer t)
  (setq highlight-nonselected-windows nil)
  (setq kill-buffer-query-functions nil)
  (setq-default cache-long-scans t)
  (setq-default word-wrap nil)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default fill-column 80)
  (setq-default line-spacing 0)
  (setq-default truncate-lines t)
  (setq enable-recursive-minibuffers t)
  (setq resize-mini-windows t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq completion-cycle-threshold 3)
  (setq completion-ignore-case t)
  (setq completions-detailed t)
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

  :hook
  ((before-save-hook . delete-trailing-whitespace))

  :config
  (dolist (mode '(recentf-mode
                  electric-indent-mode
                  show-paren-mode
                  save-place-mode
                  global-hl-line-mode
                  column-number-mode
                  winner-mode
                  global-auto-revert-mode
                  file-name-shadow-mode
                  minibuffer-depth-indicate-mode
                  minibuffer-electric-default-mode))
    (funcall mode 1))
  (blink-cursor-mode -1)
  (remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
  (advice-add #'shr-colorize-region :around (defun shr-no-colorise-region (&rest ignore)))

  :bind
  (("M-[" . beginning-of-buffer)
   ("M-]" . end-of-buffer)
   ("C-x C-r" . revert-buffer)
   ("C-x |" . j0ni/toggle-window-split)
   ("C-c C-k" . eval-buffer)
   ("C-c ." . j0ni/delete-whitespace)
   ("C-c s" . j0ni/insert-shrug)
   ("C-=" . text-scale-increase)
   ("C--" . text-scale-decrease))

  :chords
  (("df" . previous-window-any-frame)
   ("jk" . next-window-any-frame)
   ("kl" . display-line-numbers-mode)))

(use-package savehist
  :init
  (setq savehist-save-minibuffer-history t)
  (setq history-length 10000)
  (setq history-delete-duplicates t)

  :hook
  ((after-init-hook . savehist-mode)))

(use-package time
  ;; :hook ((after-init-hook . display-time-mode))
  :init
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
          ("Asia/Tokyo" "Tokyo"))))

(use-package xref
  ;;:init
  ;; WIP - I want to pop back to the buffer I was in before
  ;; (defvar j0ni/window-history-alist '() "keep track of where we are")
  ;; (defun j0ni/xref--show-xref-buffer (fetcher alist)
  ;;   (let ((this-buffer (window-buffer))
  ;;         (this-window (get-buffer-window this-buffer))
  ;;         (current-window-list (assoc-default this-window j0ni/window-history-alist nil '())))
  ;;     (append )
  ;;     (setq j0ni/window-history-alist (cons '(this-window) ))))
  ;; (lambda ()
  ;;                                    (let ((buf (window-buffer)))))
  :init
  (setq xref-marker-ring-length 64)
  (setq xref-show-xrefs-function 'xref--show-xref-buffer) ; default
  (setq xref-show-definitions-function 'xref-show-definitions-completing-read))

(defvar pragmata-pro-ligatures
  (mapcar #'car
          '(("[ERROR]"    #XE2C0)
            ("[DEBUG]"    #XE2C1)
            ("[INFO]"     #XE2C2)
            ("[WARN]"     #XE2C3)
            ("[WARNING]"  #XE2C4)
            ("[ERR]"      #XE2C5)
            ("[FATAL]"    #XE2C6)
            ("[TRACE]"    #XE2C7)
            ("[FIXME]"    #XE2C8)
            ("[TODO]"     #XE2C9)
            ("[BUG]"      #XE2CA)
            ("[NOTE]"     #XE2CB)
            ("[HACK]"     #XE2CC)
            ("[MARK]"     #XE2CD)
            ("# ERROR"    #XE2F0)
            ("# DEBUG"    #XE2F1)
            ("# INFO"     #XE2F2)
            ("# WARN"     #XE2F3)
            ("# WARNING"  #XE2F4)
            ("# ERR"      #XE2F5)
            ("# FATAL"    #XE2F6)
            ("# TRACE"    #XE2F7)
            ("# FIXME"    #XE2F8)
            ("# TODO"     #XE2F9)
            ("# BUG"      #XE2FA)
            ("# NOTE"     #XE2FB)
            ("# HACK"     #XE2FC)
            ("# MARK"     #XE2FD)
            ("// ERROR"   #XE2E0)
            ("// DEBUG"   #XE2E1)
            ("// INFO"    #XE2E2)
            ("// WARN"    #XE2E3)
            ("// WARNING" #XE2E4)
            ("// ERR"     #XE2E5)
            ("// FATAL"   #XE2E6)
            ("// TRACE"   #XE2E7)
            ("// FIXME"   #XE2E8)
            ("// TODO"    #XE2E9)
            ("// BUG"     #XE2EA)
            ("// NOTE"    #XE2EB)
            ("// HACK"    #XE2EC)
            ("// MARK"    #XE2ED)
            ("!!"         #XE900)
            ("!="         #XE901)
            ("!=="        #XE902)
            ("!!!"        #XE903)
            ("!≡"         #XE904)
            ("!≡≡"        #XE905)
            ("!>"         #XE906)
            ("!=<"        #XE907)
            ("#("         #XE920)
            ("#_"         #XE921)
            ("#{"         #XE922)
            ("#?"         #XE923)
            ("#>"         #XE924)
            ("##"         #XE925)
            ("#_("        #XE926)
            ("%="         #XE930)
            ("%>"         #XE931)
            ("%>%"        #XE932)
            ("%<%"        #XE933)
            ("&%"         #XE940)
            ("&&"         #XE941)
            ("&*"         #XE942)
            ("&+"         #XE943)
            ("&-"         #XE944)
            ("&/"         #XE945)
            ("&="         #XE946)
            ("&&&"        #XE947)
            ("&>"         #XE948)
            ("$>"         #XE955)
            ("***"        #XE960)
            ("*="         #XE961)
            ("*/"         #XE962)
            ("*>"         #XE963)
            ("++"         #XE970)
            ("+++"        #XE971)
            ("+="         #XE972)
            ("+>"         #XE973)
            ("++="        #XE974)
            ("--"         #XE980)
            ("-<"         #XE981)
            ("-<<"        #XE982)
            ("-="         #XE983)
            ("->"         #XE984)
            ("->>"        #XE985)
            ("---"        #XE986)
            ("-->"        #XE987)
            ("-+-"        #XE988)
            ("-\\/"       #XE989)
            ("-|>"        #XE98A)
            ("-<|"        #XE98B)
            (".."         #XE990)
            ("..."        #XE991)
            ("..<"        #XE992)
            (".>"         #XE993)
            (".~"         #XE994)
            (".="         #XE995)
            ("/*"         #XE9A0)
            ("//"         #XE9A1)
            ("/>"         #XE9A2)
            ("/="         #XE9A3)
            ("/=="        #XE9A4)
            ("///"        #XE9A5)
            ("/**"        #XE9A6)
            (":::"        #XE9AF)
            ("::"         #XE9B0)
            (":="         #XE9B1)
            (":≡"         #XE9B2)
            (":>"         #XE9B3)
            (":=>"        #XE9B4)
            (":("         #XE9B5)
            (":-("        #XE9B6)
            (":)"         #XE9B7)
            (":-)"        #XE9B8)
            (":/"         #XE9B9)
            (":\\"        #XE9BA)
            (":3"         #XE9BB)
            (":D"         #XE9BC)
            (":P"         #XE9BD)
            (":>:"        #XE9BE)
            (":<:"        #XE9BF)
            ("<$>"        #XE9C0)
            ("<*"         #XE9C1)
            ("<*>"        #XE9C2)
            ("<+>"        #XE9C3)
            ("<-"         #XE9C4)
            ("<<"         #XE9C5)
            ("<<<"        #XE9C6)
            ("<<="        #XE9C7)
            ("<="         #XE9C8)
            ("<=>"        #XE9C9)
            ("<>"         #XE9CA)
            ("<|>"        #XE9CB)
            ("<<-"        #XE9CC)
            ("<|"         #XE9CD)
            ("<=<"        #XE9CE)
            ("<~"         #XE9CF)
            ("<~~"        #XE9D0)
            ("<<~"        #XE9D1)
            ("<$"         #XE9D2)
            ("<+"         #XE9D3)
            ("<!>"        #XE9D4)
            ("<@>"        #XE9D5)
            ("<#>"        #XE9D6)
            ("<%>"        #XE9D7)
            ("<^>"        #XE9D8)
            ("<&>"        #XE9D9)
            ("<?>"        #XE9DA)
            ("<.>"        #XE9DB)
            ("</>"        #XE9DC)
            ("<\\>"       #XE9DD)
            ("<\">"       #XE9DE)
            ("<:>"        #XE9DF)
            ("<~>"        #XE9E0)
            ("<**>"       #XE9E1)
            ("<<^"        #XE9E2)
            ("<!"         #XE9E3)
            ("<@"         #XE9E4)
            ("<#"         #XE9E5)
            ("<%"         #XE9E6)
            ("<^"         #XE9E7)
            ("<&"         #XE9E8)
            ("<?"         #XE9E9)
            ("<."         #XE9EA)
            ("</"         #XE9EB)
            ("<\\"        #XE9EC)
            ("<\""        #XE9ED)
            ("<:"         #XE9EE)
            ("<->"        #XE9EF)
            ("<!--"       #XE9F0)
            ("<--"        #XE9F1)
            ("<~<"        #XE9F2)
            ("<==>"       #XE9F3)
            ("<|-"        #XE9F4)
            ("<<|"        #XE9F5)
            ("<-<"        #XE9F7)
            ("<-->"       #XE9F8)
            ("<<=="       #XE9F9)
            ("<=="        #XE9FA)
            ("=<<"        #XEA00)
            ("=="         #XEA01)
            ("==="        #XEA02)
            ("==>"        #XEA03)
            ("=>"         #XEA04)
            ("=~"         #XEA05)
            ("=>>"        #XEA06)
            ("=/="        #XEA07)
            ("=~="        #XEA08)
            ("==>>"       #XEA09)
            ("≡≡"         #XEA10)
            ("≡≡≡"        #XEA11)
            ("≡:≡"        #XEA12)
            (">-"         #XEA20)
            (">="         #XEA21)
            (">>"         #XEA22)
            (">>-"        #XEA23)
            (">>="        #XEA24)
            (">>>"        #XEA25)
            (">=>"        #XEA26)
            (">>^"        #XEA27)
            (">>|"        #XEA28)
            (">!="        #XEA29)
            (">->"        #XEA2A)
            ("??"         #XEA40)
            ("?~"         #XEA41)
            ("?="         #XEA42)
            ("?>"         #XEA43)
            ("???"        #XEA44)
            ("?."         #XEA45)
            ("^="         #XEA48)
            ("^."         #XEA49)
            ("^?"         #XEA4A)
            ("^.."        #XEA4B)
            ("^<<"        #XEA4C)
            ("^>>"        #XEA4D)
            ("^>"         #XEA4E)
            ("\\\\"       #XEA50)
            ("\\>"        #XEA51)
            ("\\/-"       #XEA52)
            ("@>"         #XEA57)
            ("|="         #XEA60)
            ("||"         #XEA61)
            ("|>"         #XEA62)
            ("|||"        #XEA63)
            ("|+|"        #XEA64)
            ("|->"        #XEA65)
            ("|-->"       #XEA66)
            ("|=>"        #XEA67)
            ("|==>"       #XEA68)
            ("|>-"        #XEA69)
            ("|<<"        #XEA6A)
            ("||>"        #XEA6B)
            ("|>>"        #XEA6C)
            ("|-"         #XEA6D)
            ("||-"        #XEA6E)
            ("~="         #XEA70)
            ("~>"         #XEA71)
            ("~~>"        #XEA72)
            ("~>>"        #XEA73)
            ("[["         #XEA80)
            ("]]"         #XEA81)
            ("\">"        #XEA90)
            ("_|_"        #XEA97))))

(use-package ligature
  :commands
  (ligature-set-ligatures)

  :straight
  (ligature :type git :host github :repo "mickeynp/ligature.el")

  ;; :config
  ;; Cascadia Code
  ;; (ligature-set-ligatures
  ;;  'prog-mode '("=:=" "==>" "=<<" "!!." ">>=" "->>" "-->"
  ;;               "<==" "<=>" "<--" "<<-" "::" ":=" "=>" "!="
  ;;               "--" ">=" ">>" "->" "<=" "<-" "<<" ".." "/*" "//" "__"))
  ;; (ligature-set-ligatures 'prog-mode pragmata-pro-ligatures)
  ;; :hook ((prog-mode-hook . ligature-mode))
  )

(use-package beacon
  :commands
  (beacon-blink)

  :bind
  (("C-x =" . beacon-blink)))

(use-package ibuffer
  :bind
  (("C-x C-b" . ibuffer))

  :init
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-old-time 48))

(use-package ibuffer-vc
  :hook
  ((ibuffer-hook . j0ni/ibuffer-vc-hook))

  :init
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
      (ibuffer-do-sort-by-alphabetic))))

(use-package find-file-in-project
  :commands
  (find-file-in-project-by-selected
   ffip-get-project-root-directory)

  :bind
  (("C-c f" . find-file-in-project-by-selected))

  :init
  (setq ffip-use-rust-fd t))

(use-package erc
  :init
  (setq erc-email-userid "j0ni")
  (defun j0ni/connect-srht-bouncer ()
    (interactive)
    (erc-tls
     :server "chat.sr.ht"
     :port "6697"
     :nick "j0ni"
     :full-name "Joni"
     :password "b62b1c04a20613e49aa02f201eb4c8ea")))

(use-package circe
  :init
  (setq circe-network-defaults
        '(("Libera Chat" :host "irc.libera.chat" :port 6697 :use-tls t :nick "j0ni")
          ("Sourcehut Bouncer" :host "chat.sr.ht" :port 6697 :use-tls t :nick "j0ni" :user "j0ni"
           :sasl-username "j0ni"
           :sasl-password "b62b1c04a20613e49aa02f201eb4c8ea"))))

(use-package undo-fu
  :init
  (setq undo-fu-allow-undo-in-region t)

  :bind
  (("C-_" . undo-fu-only-undo)
   ("C-/" . undo-fu-only-undo)
   ("C-z" . undo-fu-only-undo)
   ("<undo>" . undo-fu-only-undo)
   ("C-x u" . undo-fu-only-undo)
   ("M-_" . undo-fu-only-redo)
   ("C-M-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :hook
  ((after-init-hook . global-undo-fu-session-mode)))

(use-package exec-path-from-shell
  :init
  (defvar j0ni/exec-path-from-shell-completed nil "Stop this happening repeatedly")
  (when (and (not j0ni/exec-path-from-shell-completed)
             (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)
    (setq j0ni/exec-path-from-shell-completed t)))

(use-package modus-themes
  :hook
  ((after-init-hook . modus-themes-load-themes))

  :init
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-slanted-constructs t)
  (setq modus-themes-syntax '(yellow-comments)) ;; 'faint
  (setq modus-themes-fringes nil)
  (setq modus-themes-hl-line '(underline neutral))
  (setq modus-themes-completions 'opinionated)
  (setq modus-themes-scale-headings t)
  (setq modus-themes-mode-line '(accented padded))
  (setq modus-themes-paren-match '(intense bold underline))

  :config
  ;; (load-theme 'modus-operandi t)
  (load-theme 'modus-vivendi t)
  ;; if the font is paying attention ¯\_(ツ)_/¯
  (set-face-attribute 'default nil :weight 'light)
  (set-face-attribute 'bold nil :weight 'semibold))

(use-package hl-todo
  :hook
  ((after-init-hook . global-hl-todo-mode)))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode +1)
  (diminish 'volatile-highlights-mode))

(use-package doom-themes)

(use-package dracula-theme
  ;; :init (load-theme 'dracula t))
  :commands
  (dracula-theme))

(use-package almost-mono-themes)

(use-package rainbow-mode
  :bind
  (("C-c r" . rainbow-mode)))

(use-package rainbow-delimiters
  :hook
  ((paredit-mode-hook . rainbow-delimiters-mode)))

(use-package browse-at-remote)

(use-package diff-hl
  :hook
  ((after-init-hook . global-diff-hl-mode))

  :config
  (eval-after-load 'magit
    '(progn
       (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
       (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

(use-package expand-region
  :bind
  (("C-x C-x" . er/expand-region)))

(use-package dockerfile-mode)

(use-package anzu
  :commands
  (global-anzu-mode)

  :hook
  ((after-init-mode . global-anzu-mode)))

;;; Completion

(use-package company
  :hook
  ((after-init-hook . global-company-mode))

  :init
  (setq company-global-modes '(not org-mode))
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 1.0)
  (setq company-tooltip-idle-delay 1.0)
  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend
                            company-echo-metadata-frontend))

  :bind
  (("M-\\" . company-complete)
   ("C-\\" . company-complete)
   :map company-active-map
   ("M-\\" . company-complete-common-or-cycle)
   ("C-\\" . company-complete-common-or-cycle)
   ("C-j" . company-complete-selection)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)))

(use-package prescient
  :commands
  (prescient-persist-mode)

  :init
  (prescient-persist-mode 1))

(use-package company-prescient
  :hook
  ((after-init-hook . company-prescient-mode)))

(use-package orderless
  :init
  (setq orderless-matching-styles '(orderless-literal orderless-regexp))
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind
  (;; Everywhere
   ("C-s" . consult-line)
   ;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c b" . consult-bookmark)
   ("C-c k" . consult-kmacro)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ;;("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ("<help> a" . consult-apropos) ;; orig. apropos-command
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)           ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)         ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)       ;; orig. goto-line
   ("M-g o" . consult-outline)           ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings (search-map)
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
   ;; Isearch integration
   ("M-s e" . consult-isearch)
   :map isearch-mode-map
   ("M-e" . consult-isearch)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)          ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi))   ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook
  ((completion-list-mode . consult-preview-at-point-mode))

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package consult-company
  :after
  (company consult)

  :init
  (define-key company-mode-map [remap completion-at-point] #'consult-company))

(use-package embark
  :bind
  (("C-." . embark-act)  ;; pick some comfortable binding
   ("C-;" . embark-dwim) ;; good alternative: M-.
   ;; ("M-." . embark-dwim)       ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   ;; :map minibuffer-mode-map
   ;; ("M-." . embark-collect-live)
   )

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" .
                 (display-buffer-at-bottom . (window-parameters (mode-line-format . none)
                                                                (window-height . 10))))))

(use-package embark-consult
  :after
  (embark consult)

  :hook
  ((embark-collect-mode-hook . consult-preview-at-point-mode)))

(use-package marginalia
  :commands
  (marginalia-mode)

  :init
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light))

  :hook
  ((after-init-hook . marginalia-mode)))

;; Choose a framework
(cl-case j0ni/completion-system
  ('selectrum (require 'selectrum-support))
  ('vertico (require 'vertico-support))
  ('icomplete (require 'icomplete-support))
  ('mct (require 'mct-support))
  (t (message "Completion system [j0ni/completion-system] not set, nothing configured")))

(use-package browse-kill-ring
  :init
  (browse-kill-ring-default-keybindings))

(use-package magit
  :init
  (setq magit-completing-read-function #'completing-read)
  (setq magit-diff-refine-hunk t)
  (setq magit-bury-buffer-function #'magit-mode-quit-window)

  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)))

(use-package idle-highlight
  :hook
  ((prog-mode-hook . idle-highlight)))

(use-package projectile
  :hook
  ((after-init-hook . projectile-mode))

  :bind-keymap
  ("C-c p" . projectile-command-map))

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
  :hook
  ((emacs-lisp-mode-hook . enable-paredit-mode)
   (lisp-mode-hook . enable-paredit-mode)
   (scheme-mode-hook . enable-paredit-mode))

  :commands
  (enable-paredit-mode)

  :config
  (define-key paredit-mode-map (kbd "C-M-s") #'paredit-splice-sexp)
  (define-key paredit-mode-map (kbd "M-s") nil))

(use-package geiser
  :commands
  (turn-on-geiser-mode)

  :hook
  ((scheme-mode-hook . turn-on-geiser-mode)
   (geiser-repl-mode-hook . enable-paredit-mode)))

(use-package geiser-chicken)
(use-package geiser-guile)
(use-package geiser-racket)
(use-package geiser-chez)

(use-package racket-mode
  :hook
  ((racket-mode-hook . enable-paredit-mode)
   (racket-mode-hook . turn-on-geiser-mode)))

(use-package which-key
  :hook
  (after-init-hook . which-key-mode))

(use-package switch-window
  :init
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-shortcut-appearance 'text)
  (setq switch-window-auto-resize-window nil)
  (setq switch-window-background t)
  (setq switch-window-default-window-size 0.8)

  :hook
  ((after-init-hook . switch-window-mouse-mode))

  :commands
  (switch-window switch-window-mouse-mode)

  :bind
  (("C-x o" . switch-window)))

(use-package yaml-mode)

(use-package web-mode
  :init
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
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

(use-package restclient)

(use-package lsp-mode
  :commands
  (lsp lsp-register-custom-settings lsp-deferred)

  :hook
  ((lsp-mode-hook . lsp-enable-which-key-integration))

  :init
  (setq lsp-auto-configure nil)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-file-watchers t)
  (setq lsp-enable-links t)
  (setq lsp-enable-imenu t)
  (setq lsp-enable-dap-auto-configure t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-xref t)
  (setq lsp-enable-indentation t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-workspace-status-enable nil)
  (setq lsp-completion-enable t)
  (setq lsp-auto-guess-root nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-signature-render-all t)
  (setq lsp-idle-delay 0.8)
  (setq lsp-lens-enable nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-file-watch-threshold 10000)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-keymap-prefix "C-c l")

  :bind
  (:map lsp-mode-map
        ("C-c C-l d" . lsp-describe-thing-at-point)
        ("C-c C-l e" . lsp-execute-code-action)
        ("C-c C-l r" . lsp-rename)
        ("C-c C-l i" . lsp-find-implementation)
        ("C-c C-l ." . lsp-find-type-definition)
        ("C-c C-l x" . lsp-workspace-restart)))

;;; Scala

(use-package lsp-metals)

(use-package scala-mode
  :hook
  ((scala-mode-hook . lsp))

  :interpreter
  ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands
  (sbt-start sbt-command)

  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-java
  :hook
  ((java-mode-hook . lsp)))

(use-package lsp-ui
  :config
  (require 'lsp-ui-imenu)

  :init
  (setq lsp-ui-autoconfigure t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-always-show nil)
  (setq lsp-ui-imenu-autorefresh t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse nil)

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

(use-package treemacs
  :init
  (setq treemacs-space-between-root-nodes nil))

(use-package lsp-treemacs)

(use-package sly
  :hook
  ((sly-mrepl-hook . company-mode))

  :init
  (setq inferior-lisp-program "sbcl"))

(use-package sly-quicklisp)
(use-package sly-macrostep)
(use-package sly-asdf)

;; (use-package inf-clojure
;;   ;; :config
;;   ;; (inf-clojure-update-feature 'clojure 'completion
;;   ;;                             "(compliment.core/completions \"%s\")")
;;   :hook ((inf-clojure-mode-hook . turn-on-eldoc-mode)
;;          (inf-clojure-mode-hook . enable-paredit-mode)))

(use-package cider
  :commands
  (cider-mode)

  :hook
  ((cider-mode-hook . turn-on-eldoc-mode)
   (cider-repl-mode-hook . enable-paredit-mode))

  :init
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-save-file-on-load t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-use-overlays t)                ; display eval results inline
  (setq cider-use-fringe-indicators nil)
  (setq cider-stacktrace-default-filters '(tooling dup))
  (setq cider-repl-history-size 10000)
  (setq cider-prompt-for-symbol nil)
  (setq cider-known-endpoints nil)
  (setq cider-repl-history-file (concat user-emacs-directory ".cider-repl-history"))
  (setq cider-prefer-local-resources t)
  (setq cider-inject-dependencies-at-jack-in t)
  (setq cider-eldoc-display-context-dependent-info t)
  (setq cider-auto-mode t)

  :bind
  (:map cider-repl-mode-map
        ("RET" . cider-repl-newline-and-indent)
        ("C-RET" . cider-repl-return)))

(use-package cider-eval-sexp-fu)

(use-package clj-refactor
  :commands
  (clj-refactor-mode)

  :init
  (setq cljr-add-ns-to-blank-clj-files t)
  (setq cljr-warn-on-eval nil)
  (setq cljr-suppress-middleware-warnings t)
  (setq cljr-favor-prefix-notation nil)
  (setq cljr-favor-private-functions nil)
  (setq cljr-inject-dependencies-at-jack-in t)
  (setq cljr-eagerly-build-asts-on-startup nil)
  (setq cljr-ignore-analyzer-errors t)

  :config
  (cljr-add-keybindings-with-prefix "C-c C-j"))

(use-package clojure-mode
  :init
  ;; (setq lsp-clojure-custom-server-command '("bash" "-c" "/usr/bin/clojure-lsp"))
  (defun j0ni/clojure-hook ()
    (cider-mode 1)
    (clj-refactor-mode 1)
    (enable-paredit-mode)
    ;; (flycheck-mode 1)
    (subword-mode 1)
    ;; (require 'lsp-clojure)
    ;; (lsp)
    )

  :hook
  (((clojure-mode-hook
     clojurec-mode-hook
     clojurescript-mode-hook
     clojurex-mode-hook)
    . j0ni/clojure-hook)))

(use-package ruby-mode
  :hook
  (ruby-mode-hook . flycheck-mode))

(use-package inf-ruby)

(use-package rbenv
  :init
  (setq rbenv-show-active-ruby-in-modeline nil)

  :commands
  (global-rbenv-mode rbenv-use-corresponding rbenv-use)

  :hook
  ((after-init-hook . global-rbenv-mode)
   (ruby-mode-hook . rbenv-use-corresponding)))

(use-package json-mode)
(use-package graphql-mode)

(use-package purescript-mode
  :hook
  ((purescript-mode-hook . turn-on-purescript-indentation)))

(use-package psc-ide
  :hook
  ((purescript-mode-hook . psc-ide-mode)))

(use-package typescript-mode
  :after
  (lsp-mode)

  :hook
  ((typescript-mode-hook . lsp)))

(use-package eros
  :hook
  ((after-init-hook . eros-mode)))

(use-package flycheck
  :hook
  ((prog-mode-hook . flycheck-mode))

  :init
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc clojure clojurescript))
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))

  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(use-package rustic
  :commands
  (rustic-mode)

  :hook
  ((rustic-mode-hook . lsp)
   (rustic-mode-hook . electric-pair-mode))

  :init
  (setq rustic-format-trigger 'on-save)
  (setq rustic-indent-method-chain t)
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-lsp-format t)
  (setq rustic-indent-method-chain nil)
  (setq lsp-rust-server 'rust-analyzer)

  :config
  (require 'lsp-rust)
  (push 'rustic-clippy flycheck-checkers))

(use-package eglot
  :commands
  (eglot-ensure))

(use-package org
  :straight
  (:type built-in)

  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda))

  :init
  (setq org-startup-indented t)
  ;; make it short to start with
  (setq org-startup-folded t)
  ;; where things live
  (setq org-directory "~/Dropbox/OrgMode/")
  ;; Set agenda file(s)
  (setq org-agenda-files (list (concat org-directory "journal.org")
                               (concat org-directory "berlin.org")
                               (concat org-directory "shrieks.org")))
  (setq org-agenda-span 14)
  ;; prevent org-mode hijacking arrow keys
  (setq org-replace-disputed-keys t)
  ;; set our own todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w!)" "PAUSED(p!)" "|" "DONE(d!)" "ABANDONED(a!)")))
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
  (setq org-use-fast-todo-selection 'auto)
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
  (setq org-edit-src-content-indentation 0)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-default-notes-file (concat org-directory "/berlin.org"))
  (setq org-capture-templates
        `(("j" "Journal" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
           "* %T\n%?\n\n%a")
          ("s" "Shriek" entry (file+headline ,(concat org-directory "/shrieks.org") "Shrieks")
           "* %T\n%?\n")
          ("t" "Task" entry (file+headline ,(concat org-directory "/berlin.org") "Inbox")
           "* TODO %?\n  %a\n%i")
          ("b" "BP Journal" entry (file+olp+datetree ,(concat org-directory "/bp.org") "Blood Pressure")
           "* %T\n** Systolic: %^{systolic}\n** Diastolic: %^{diastolic}\n** Pulse: %^{pulse}\n** Notes\n%?\n")))
  ;; (org-publish-project-alist
  ;;  '(("")))
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

  :hook
  ((org-mode-hook . j0ni/org-mode-hook)
   (org-capture-mode-hook . j0ni/org-mode-hook)))

(use-package simple-httpd)

(use-package org-super-agenda
  :after
  (org-with-contrib)

  :init
  (setq org-super-agenda-groups '((:auto-dir-name t)))

  :hook
  ((org-agenda-mode-hook . org-super-agenda-mode)))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (expand-file-name "org-roam" org-directory))

  :config
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(use-package elfeed
  :init
  (setq elfeed-feeds '("https://pluralistic.net/feed/")))

(use-package telega
  :commands
  (telega
   telega-mode-line-mode
   telega-notifications-mode)

  :bind
  (("C-x M-t" . telega))

  :hook
  ((telega-chat-mode-hook . visual-line-mode))

  :init
  (when j0ni/is-mac
    (setq telega-server-libs-prefix "/opt/homebrew"))

  :config
  (telega-mode-line-mode t)
  (telega-notifications-mode t)
  (eval-after-load 'modus-themes
    '(defadvice modus-themes-toggle (after clear-telega-icon-cache activate)
       (setq telega-mode-line--logo-image-cache nil))))

;; (use-package doom-modeline
;;   ;; :init
;;   ;; (setq image-scaling-factor 1.4)
;;   :custom
;;   (doom-modeline-hud t)
;;   (doom-modeline-icon nil)
;;   (doom-modeline-window-width-limit nil)
;;   (doom-modeline-bar-width 4)
;;   :hook ((after-init-hook . doom-modeline-mode)))

(use-package minions
  :hook
  ((after-init-hook . minions-mode)))

(use-package markdown-mode
  :hook
  ((markdown-mode-hook . visual-line-mode)))

(use-package all-the-icons
  :commands
  (all-the-icons-install-fonts))

(use-package haskell-mode
  :hook
  ((haskell-mode-hook . electric-pair-mode)))

(use-package olivetti
  :init
  (setq olivetti-body-width 120))

;; mu4e isn't packaged in the usual way, it gets installed as part of the `mu` system package.

(defvar j0ni/mu4e-path)

(if j0ni/is-mac
    (setq j0ni/mu4e-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
  (setq j0ni/mu4e-path "/usr/share/emacs/site-lisp/mu4e"))

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
                     (mu4e-compose-signature . "In this world / we walk on the roof of hell / gazing at flowers\n    - Kobayashi Issa\n\nhttps://j0ni.ca ~ https://keybase.io/j0ni")
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

;; everything is now loaded...
(add-hook 'after-init-hook #'server-start)
