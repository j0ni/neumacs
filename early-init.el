;;; early-init.el --- early config for emacs

(defvar j0ni/fixed-font nil
  "Should be a string like \"Fira Code Mono-11\" or such.")

(defvar j0ni/variable-font nil
  "Should be a string like \"Fira Code-11\" or such.")

;;; Fonty fonty fonty fonty fonty LEAVE ME ALONE fonty fonty fonty

(setq j0ni/fixed-font "AurulentSansMono Nerd Font Mono-9.0")
;; (setq j0ni/fixed-font (font-spec :family "Iosevka Nerd Font" :size 13.5 :antialias t))
(setq j0ni/fixed-font "Fira Code Nerd Font-9.5")
;; (setq j0ni/fixed-font "FuraMono Nerd Font Mono-9.0")
;; (setq j0ni/fixed-font (font-spec :family "Monoisome" :size 9.5 :antialias t))
;; (setq j0ni/fixed-font (font-spec :family "Agave Nerd Font" :size 16.0 :antialias t))
(setq j0ni/fixed-font "Lucida Grande Mono Nrw-11.0")
;; (setq j0ni/fixed-font (font-spec :family "TerminessTTF Nerd Font Mono" :size 16.5 :antialias t))
;; (setq j0ni/fixed-font (font-spec :family "Latin Modern Mono" :size 15.0 :antialias t))
;; (setq j0ni/fixed-font (font-spec :family "BlexMono Nerd Font Mono" :size 12.5 :antialias t))
;; (setq j0ni/fixed-font (font-spec :family "Anonymice Nerd Font Mono" :size 11.5))
;; (setq j0ni/fixed-font (font-spec :family "D2Coding" :size 11.5 :antialias t :hinting t :spacing 'm))
;; (setq j0ni/fixed-font "Envy Code R-9.0")
;; (setq j0ni/fixed-font "GoMono Nerd Font Mono-9.0")
;; (setq j0ni/fixed-font (font-spec :family "PragmataPro Mono Liga" :size 11.5 :antialias t :hinting "full" :spacing 'm))
;; (setq j0ni/fixed-font (font-spec :family "Inconsolata Nerd Font" :size 13.5 :spacing 'm))
;; (setq j0ni/fixed-font "Inconsolata Nerd Font-13.0")
;; (setq j0ni/fixed-font (font-spec :family "Lucida Grande Mono" :size 11.0 :antialias t))
(setq j0ni/variable-font "Lucida Grande-11.0")

(set-frame-font j0ni/fixed-font nil t)

(menu-bar-mode -1)

(fringe-mode 8)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(defun j0ni/init-frame (&optional frame)
  (interactive)
  (unless frame (setq frame (selected-frame)))

  (set-face-attribute 'default nil :font j0ni/fixed-font)
  (set-face-attribute 'mode-line nil :font j0ni/fixed-font)
  (set-frame-font j0ni/fixed-font nil (list frame))
  (set-face-font 'variable-pitch j0ni/variable-font frame)
  (set-face-font 'fixed-pitch j0ni/fixed-font frame)
  (set-face-font 'fixed-pitch-serif j0ni/fixed-font frame)
  (set-fontset-font t 'unicode "Symbola" frame 'prepend)
  ;; if the font is paying attention ¯\_(ツ)_/¯
  ;; (set-face-attribute
  ;;  'default frame
  ;;  :weight 'semi-light)
  (set-face-attribute
   'bold frame
   :weight 'semi-bold)

  (setq x-underline-at-descent-line t)

  ;; for when it matters
  (setq mouse-autoselect-window t)
  (setq focus-follows-mouse t))

;; This is an attempt to prevent the emacsclient frame from ignoring all this
;; stuff. Unfortunately it does not appear to work. ¯\_(ツ)_/¯
;; default value
;; (setq after-make-frame-functions '(select-frame exwm-init))
(add-to-list 'after-make-frame-functions #'j0ni/init-frame)
(add-to-list 'default-frame-alist `(font . ,j0ni/fixed-font))
(add-hook 'after-init-hook #'j0ni/init-frame)
