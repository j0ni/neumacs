;;; bandali-erc.el --- bandali's ERC setup           -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020  Amin Bandali

;; Author: Amin Bandali <bandali@gnu.org>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My ERC setup for IRC.  It uses my fork of ZNC.el.

;;; Code:

(defvar j0ni/erc-detach-on-kill t)

(with-eval-after-load 'erc
  (make-directory (concat user-emacs-directory "erc/dcc/") t)
  (setq erc-auto-query 'bury
        erc-autojoin-domain-only nil
        erc-dcc-get-default-directory (concat user-emacs-directory "erc/dcc")
        erc-email-userid "joni"
        erc-format-nick-function 'erc-format-@nick
        erc-join-buffer 'bury
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-nick "joni"
        erc-prompt "erc>"
        erc-prompt-for-password nil
        erc-query-display 'buffer
        erc-rename-buffers t
        erc-server-reconnect-attempts 5
        erc-server-reconnect-timeout 3)

  (declare-function erc-message "erc-backend"
                    (message-command line &optional force))
  (declare-function erc-default-target "erc")
  (declare-function erc-current-nick "erc")
  (declare-function erc-cmd-DEOP "erc" (&rest people))
  (add-to-list 'erc-modules 'keep-place)
  (add-to-list 'erc-modules 'log)
  (when (display-graphic-p)
    (add-to-list 'erc-modules 'notifications)
    (add-to-list 'erc-modules 'smiley))
  ;; (add-to-list 'erc-modules 'spelling)
  (declare-function erc-update-modules "erc")
  (erc-update-modules)

  ;; (set-face-attribute
  ;;  'erc-nick-default-face nil
  ;;  ;; :weight 'semibold
  ;;  ;; :background "#f2f2f2"
  ;;  ;; :foreground "#222222"
  ;;  :weight 'bold
  ;;  :background "#f8f8f8"
  ;;  :foreground "#6a6a6a")

  ;; (set-face-attribute
  ;;  'erc-notice-face nil
  ;;  ;; :background "#fffadf"
  ;;  ;; :background "#f9f9f9"
  ;;  :background 'unspecified
  ;;  ;; :foreground "#809de5"
  ;;  :foreground "steel blue")

  ;; erc-fill
  ;; (csetq
  ;;  erc-fill-column 77
  ;;  erc-fill-function 'erc-fill-variable
  ;;  erc-fill-static-center 18)
  ;; to disable:
  ;; (erc-fill-mode -1)

  ;; erc-log
  (setq
   ;; erc-enable-logging 'erc-log-all-but-server-buffers
   erc-generate-log-file-name-function 'erc-generate-log-file-name-with-date
   erc-log-channels-directory (concat user-emacs-directory "erc/logs")
   erc-log-file-coding-system 'utf-8
   erc-log-write-after-insert t
   erc-log-write-after-send t
   erc-save-buffer-on-part nil
   erc-save-queries-on-quit nil)

  ;; erc-match
  (setq erc-pal-highlight-type 'nick)

  ;; erc-stamp
  (setq erc-timestamp-only-if-changed-flag nil
        erc-timestamp-format "%T "
        erc-insert-timestamp-function 'erc-insert-timestamp-left)

  (with-eval-after-load 'erc-match
    (set-face-attribute
     'erc-timestamp-face nil
     :foreground "#aaeeaa"
     :weight 'unspecified
     :background 'unspecified))

  ;; erc-track
  (setq erc-track-enable-keybindings nil
        erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                  "324" "329" "332" "333" "353" "477")
        erc-track-position-in-mode-line t
        erc-track-priority-faces-only 'all
        erc-track-shorten-function nil
        erc-track-showcount t)

  ;; hooks
  (defun j0ni/erc-detach-or-kill-channel ()
    (if j0ni/erc-detach-on-kill
        (when (erc-server-process-alive)
          (let ((tgt (erc-default-target)))
            (erc-server-send (format "DETACH %s" tgt) nil tgt)))
      (erc-kill-channel)))
  (add-hook 'erc-kill-channel-hook #'j0ni/erc-detach-or-kill-channel)
  (remove-hook 'erc-kill-channel-hook #'erc-kill-channel))

(provide 'bandali-erc)
;;; bandali-erc.el ends here
