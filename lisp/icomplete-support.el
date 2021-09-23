;;; icomplete-support.el --- support icomplete-vertical completion system, with help

(require 'use-package)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-show-help nil
      tab-always-indent 'complete)

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

;; Switch on
(use-package icomplete
  :custom
  (icomplete-prospects-height 10)
  :commands (icomplete-vertical-mode)
  :init
  (icomplete-vertical-mode 1))

(provide 'icomplete-support)
