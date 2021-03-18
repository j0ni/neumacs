;;; boot.el --- Ensure straight and use-package are primed
;;;
;;; Commentary:
;;;
;;; This sets up the straight package management system, which is more
;;; predictable and less confusing than the package.el system, which accumulates
;;; flakiness over time.
;;;
;;; Code:

(setq comp-async-report-warnings-errors nil)

(setq straight-repository-branch "develop")
(setq straight-vc-git-default-protocol 'ssh)
(setq straight-use-package-by-default t)
(setq straight-fix-flycheck t)
(setq straight-fix-org t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'diminish)
(straight-use-package 'use-package)

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)  ; ESSENTIAL for `straight.el'
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(provide 'boot)
;;; boot.el ends here
