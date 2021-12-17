;;; vertico-support.el --- Install the vertico completion ecosystem

(straight-use-package
 '(vertico :host github
           :repo "minad/vertico"
           :files ("*" (:exclude ".git") "extensions/*.el")))

(keymap-set vertico-map "M-a" #'marginalia-cycle)

(setq vertico-count 12)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Set up vertico buffer mode
(setq vertico-buffer-display-action
      `(display-buffer-at-bottom
        (window-min-height . ,(abs (- vertico-count 2)))
        (window-height . ,(+ 3 vertico-count))))

;; switch it on
(vertico-buffer-mode 1)
(vertico-mode 1)

(provide 'vertico-support)
