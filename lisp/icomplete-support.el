;;; icomplete-support.el --- support icomplete-vertical completion system, with help

(require 'use-package)

(require 'ecm)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-show-help nil
      tab-always-indent 'complete)

;; Switch on
(use-package icomplete
  :custom
  (icomplete-prospects-height 10)
  :commands (icomplete-vertical-mode)
  :init
  (icomplete-vertical-mode 1))

(provide 'icomplete-support)
