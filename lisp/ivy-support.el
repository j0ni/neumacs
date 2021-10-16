;;; ivy-support.el --- Install the Ivy completion ecosystem

(use-package ivy
  :diminish
  :custom
  (ivy-height 15)
  (ivy-wrap t)
  (ivy-use-virtual-buffers t)
  (ivy-extra-directories nil)
  (confirm-nonexistent-file-or-buffer t)
  :init
  (ivy-mode 1)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  (setq completion-cycle-threshold 3)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-show-help nil)
  (setq completion-auto-help nil)
  (setq completions-format 'one-column)
  (setq completions-detailed t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-answer-short t)
  (setq completion-category-defaults nil)
  (setq completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp
  (setq read-buffer-completion-ignore-case t)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)
  (setq echo-keystrokes 0.25)         ; from the C source code
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c C-r" . ivy-resume)))

(use-package ivy-prescient
  :commands (ivy-presient-mode)
  :init
  (ivy-prescient-mode 1))

(use-package ivy-rich
  :commands (ivy-rich-mode)
  :after (ivy counsel)
  :init
  (ivy-rich-mode 1))

(use-package hydra
  :commands (hydra-add-imenu)
  :hook (emacs-lisp-mode . hydra-add-imenu))

(use-package ivy-hydra
  :commands (hydra-ivy/body))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("C-c u" . swiper-all)))

(use-package counsel
  :after (ivy)
  :init
  (counsel-mode 1)
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-M-y" . counsel-yank-pop)
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

(use-package counsel-jq)
(use-package counsel-org-clock)

(use-package counsel-projectile
  :diminish
  :commands (counsel-projectile-mode)
  :hook (after-init-hook . counsel-projectile-mode))

(provide 'ivy-support)
