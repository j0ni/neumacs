;;; selectrum-support.el --- Install the selectrum ecosystem

(require 'use-package)

(use-package selectrum
  :commands
  (selectrum-mode)

  :bind
  (:map selectrum-minibuffer-map
        ("C-j" . selectrum-select-current-candidate)
        ("C-s" . selectrum-previous-candidate))

  :init
  (setq selectrum-display-action nil)
  (setq selectrum-max-window-height 20)
  (setq selectrum-fix-vertical-window-height nil)
  (setq selectrum-num-candidates-displayed 'auto)
  (setq selectrum-extend-current-candidate-highlight t)

  :hook
  ((after-init-hook . selectrum-mode)))

(use-package selectrum-prescient
  :commands
  (selectrum-prescient-mode)

  :hook
  ((selectrum-mode-hook . selectrum-prescient-mode)))

(provide 'selectrum-support)
