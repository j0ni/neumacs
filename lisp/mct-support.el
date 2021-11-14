;;; mct-support.el --- Use Prot's native support hacks

(require 'use-package)

(use-package mct
  :hook
  ((after-init-hook . mct-mode))

  :straight
  (mct.el :type git :host gitlab :repo "protesilaos/mct.el")

  :init
  (setq mct-remove-shadowed-file-names t) ; works when `file-name-shadow-mode' is enabled
  (setq mct-hide-completion-mode-line nil)
  (setq mct-show-completion-line-numbers nil)
  (setq mct-apply-completion-stripes t)

  (setq mct-live-completion 'visible)
  (setq mct-minimum-input 3)
  (setq mct-live-update-delay 0.2)

  ;; NOTE: setting this variable with `setq', requires `mct-mode' to be
  ;; reloaded.
  (setq mct-completions-format 'one-column)

  ;; NOTE: `mct-completion-blocklist' can be used for commands with lots
  ;; of candidates, depending also on how low `mct-minimum-input' is.
  ;; With the settings shown here this is not required, otherwise I would
  ;; use something like this:
  ;;
  ;; (setq mct-completion-blocklist
  ;;       '( describe-symbol describe-function describe-variable
  ;;          execute-extended-command insert-char))
  (setq mct-completion-blocklist nil)

  ;; This is for commands that should always pop up the completions'
  ;; buffer.  It circumvents the default method of waiting for some user
  ;; input (see `mct-minimum-input') before displaying and updating the
  ;; completions' buffer.
  (setq mct-completion-passlist
        '(imenu
          Info-goto-node
          Info-index
          Info-menu
          vc-retrieve-tag
          projectile-switch-project
          ;; consult-buffer
          ))

  ;; You can place the Completions' buffer wherever you want, by following
  ;; the syntax of `display-buffer'.  For example, try this:

  ;; (setq mct-display-buffer-action
  ;;       (quote ((display-buffer-reuse-window
  ;;                display-buffer-in-side-window)
  ;;               (side . left)
  ;;               (slot . 99)
  ;;               (window-width . 0.3))))
  )

(provide 'mct-support)
