;;; selectrum-support.el --- Install the selectrum ecosystem

(use-package selectrum
  :commands (selectrum-mode)
  :bind (:map selectrum-minibuffer-map
              ("C-j" . selectrum-select-current-candidate)
              ("C-s" . selectrum-previous-candidate))
  :custom
  (selectrum-display-action nil)
  (selectrum-max-window-height 20)
  (selectrum-fix-vertical-window-height nil)
  (selectrum-num-candidates-displayed 'auto)
  (selectrum-extend-current-candidate-highlight t)
  :init
  (selectrum-mode 1))

(use-package selectrum-prescient
  :commands (selectrum-prescient-mode)
  :hook ((selectrum-mode-hook . selectrum-prescient-mode)))

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

(use-package marginalia
  :commands (marginalia-mode)
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light))
  :init
  (marginalia-mode 1))

(provide 'selectrum-support)
