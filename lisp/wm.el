;;; wm.el --- start the window manager -*- lexical-binding: t -*-

(add-to-list 'load-path (concat user-emacs-directory "exwm"))
(add-to-list 'load-path (concat user-emacs-directory "xelb"))

(defun j0ni-init-exwm ()
  "Initialize EXWM"
  (require 'exwm nil t)

  (setq exwm-workspace-number 9
        exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t
        exwm-manage-force-tiling t)

  (display-time-mode 1)

  (exwm-input-set-key (kbd "s-b") 'consult-buffer)
  (exwm-input-set-key (kbd "s-o") 'consult-buffer-other-window)

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Global keybindings.
  (setq exwm-input-global-keys
        `(
          ;; 's-r': Reset (to line-mode).
          ([?\s-r] . exwm-reset)
          ;; 's-w': Switch workspace.
          ([?\s-w] . exwm-workspace-switch)
          ;; 's-&': Launch application.
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ;; 's-N': Switch to certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  ;; Line-editing shortcuts
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])))
  ;; Enable EXWM
  (exwm-enable)

  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1"))
  (exwm-randr-enable))

(provide 'wm)
