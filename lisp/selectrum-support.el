;;; selectrum-support.el --- Install the selectrum ecosystem

(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)

(keymap-set selectrum-minibuffer-map "C-j" #'selectrum-select-current-candidate)
(keymap-set selectrum-minibuffer-map "C-s" #'selectrum-previous-candidate)

(setq selectrum-display-action nil)
(setq selectrum-max-window-height 20)
(setq selectrum-fix-vertical-window-height nil)
(setq selectrum-num-candidates-displayed 'auto)
(setq selectrum-extend-current-candidate-highlight t)

(selectrum-mode 1)
(selectrum-prescient-mode 1)

(provide 'selectrum-support)
