;;; wm.el --- start the window manager -*- lexical-binding: t -*-

(add-to-list 'load-path (concat user-emacs-directory "exwm"))
(add-to-list 'load-path (concat user-emacs-directory "xelb"))

(require 'exwm)
(require 'exwm-randr)

(defun j0ni/exwm-enable ()
  (interactive)
  (exwm-enable))

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(setq exwm-workspace-number 9)
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)
(setq exwm-manage-force-tiling t)

(setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset)
        ([?\s-w] . exwm-workspace-switch)
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
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

(with-eval-after-load 'exwm
  (exwm-input-set-key (kbd "s-b") 'consult-buffer)
  (exwm-input-set-key (kbd "s-o") 'consult-buffer-other-window))

(provide 'wm)
