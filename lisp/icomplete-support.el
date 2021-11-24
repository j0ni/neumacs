;;; icomplete-support.el --- support icomplete-vertical completion system, with help

(require 'use-package)

(use-package icomplete
  :init
  (setq icomplete-prospects-height 1)

  :commands
  (fido-mode fido-vertical-mode)

  :hook
  ((after-init-hook . fido-mode)))

(provide 'icomplete-support)
