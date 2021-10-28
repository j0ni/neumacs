;;; icomplete-support.el --- support icomplete-vertical completion system, with help

(require 'use-package)

(use-package icomplete
  :init
  (setq icomplete-prospects-height 10)

  :commands
  (icomplete-vertical-mode)

  :hook
  ((after-init-hook . icomplete-vertical-mode)))

(provide 'icomplete-support)
