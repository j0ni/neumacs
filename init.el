(let ((gc-cons-threshold most-positive-fixnum))
  ;; load the package manager (straight.el)
  (add-to-list 'load-path (concat user-emacs-directory "lisp"))

  (setq straight-repository-branch "develop")
  (setq straight-vc-git-default-protocol 'ssh)
  (setq straight-use-package-by-default t)
  (setq straight-fix-flycheck t)
  (setq straight-fix-org t)

  ;; if this doesn't get reset on a reload, there will be collisions
  (setq straight-recipe-overrides nil)

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

  (straight-use-package 'org)
  (straight-use-package 'org-contrib)
  (require 'org)
  ;; tangle the rest of the configuration
  (let ((init-source-file (expand-file-name "notwithstanding.org" user-emacs-directory)))
    (org-babel-load-file init-source-file)
    (org-publish "notwithstanding")
    (garbage-collect)))
