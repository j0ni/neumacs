;;; builtin-support --- config for the builtin completion bits

;;;;; abbrevations and completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-styles '(basic initials partial-completion substring)
      completion-category-defaults nil ; ehh
      completion-show-help nil
      tab-always-indent 'complete)

(provide 'builtin-support)
