;;; vertico-support.el --- Install the vertico completion ecosystem

(require 'use-package)

(use-package vertico
  :custom
  (vertico-count 10)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; switch it on
  (vertico-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(substring orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("C-r" . consult-isearch)
         ("C-c i" . consult-imenu)
         ("C-c C-s" . consult-ripgrep)
         :map projectile-command-map
         ("s r" . consult-ripgrep))
  :custom
  (consult-project-root-function #'ffip-get-project-root-directory)
  (consult-preview-key 'any)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)        ;; pick some comfortable binding
   ("C-;" . embark-dwim)       ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   :map minibuffer-mode-map
   ("M-." . embark-collect-live))
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

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package marginalia
  :commands (marginalia-mode)
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light))
  :init
  (marginalia-mode 1))

(provide 'vertico-support)
