;;; boot.el --- Ensure straight and use-package are primed
;;;
;;; Commentary:
;;;
;;; This sets up the straight package management system, which is more
;;; predictable and less confusing than the package.el system, which accumulates
;;; flakiness over time.
;;;
;;; Code:

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

(straight-use-package 'use-package)

(provide 'boot)
;;; boot.el ends here
