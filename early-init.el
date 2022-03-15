;;; early-init.el --- early config for emacs

(defvar j0ni/fixed-font nil
  "Should be a string like \"Fira Code Mono-11\" or such.")

(defvar j0ni/fixed-font-serif nil
  "Should be a string like \"Fira Code Mono-11\" or such.")

(defvar j0ni/variable-font nil
  "Should be a string like \"Fira Code-11\" or such.")

;;; Fonty fonty fonty fonty fonty LEAVE ME ALONE fonty fonty fonty

(setq j0ni/fixed-font "AurulentSansMono Nerd Font Mono-9.0")
;; (setq j0ni/fixed-font (font-spec :family "Iosevka Nerd Font" :size 13.5 :antialias t))
(setq j0ni/fixed-font "Fira Code-10.0")
;; (setq j0ni/fixed-font "FuraMono Nerd Font Mono-9.0")
(setq j0ni/fixed-font "Monoisome-8.5")
;; (setq j0ni/fixed-font (font-spec :family "Agave Nerd Font" :size 16.0 :antialias t))
(setq j0ni/fixed-font "Lucida Grande Mono Nrw-10.5")
;; (setq j0ni/fixed-font (font-spec :family "TerminessTTF Nerd Font Mono" :size 16.5 :antialias t))
;; (setq j0ni/fixed-font (font-spec :family "Latin Modern Mono" :size 15.0 :antialias t))
;; (setq j0ni/fixed-font (font-spec :family "BlexMono Nerd Font Mono" :size 12.5 :antialias t))
;; (setq j0ni/fixed-font (font-spec :family "Anonymice Nerd Font Mono" :size 11.5))
;; (setq j0ni/fixed-font (font-spec :family "D2Coding" :size 11.5 :antialias t :hinting t :spacing 'm))
;; (setq j0ni/fixed-font "Envy Code R-9.0")
;; (setq j0ni/fixed-font "GoMono Nerd Font Mono-9.0")
;; (setq j0ni/fixed-font (font-spec :family "PragmataPro Mono Liga" :size 11.5 :antialias t :hinting "full" :spacing 'm))
;; (setq j0ni/fixed-font "PragmataPro-12.0")
;; (setq j0ni/fixed-font (font-spec :family "Inconsolata Nerd Font" :size 13.5 :spacing 'm))
;; (setq j0ni/fixed-font "Inconsolata Nerd Font-13.0")
(setq j0ni/fixed-font "Recursive Mono Linear Static-12.5")
(setq j0ni/fixed-font-serif "Recursive Mono Casual Static-12.5")
;; (setq j0ni/fixed-font (font-spec :family "Lucida Grande Mono" :size 11.0 :antialias t))
(setq j0ni/variable-font "Recursive Sans Casual Static-12.5")

(setq j0ni/fixed-font "Iosevka Comfy-12.5")
(setq j0ni/fixed-font-serif "Iosevka Comfy-12.5")
;; (setq j0ni/fixed-font (font-spec :family "Lucida Grande Mono" :size 11.0 :antialias t))
(setq j0ni/variable-font "Recursive Sans Casual Static-12.5")


(set-frame-font j0ni/fixed-font nil t)

(defun j0ni/init-frame (&optional frame)
  (interactive)
  (unless frame (setq frame (selected-frame)))

  ;; All of these? Really?
  (set-frame-font j0ni/fixed-font nil (list frame))

  (set-face-attribute 'default frame :font j0ni/fixed-font)
  (set-face-attribute 'mode-line frame :font j0ni/fixed-font)
  (set-face-attribute 'fixed-pitch frame :font j0ni/fixed-font)
  (set-face-attribute 'variable-pitch frame :font j0ni/variable-font)
  (set-face-attribute 'fixed-pitch-serif frame :font j0ni/fixed-font-serif)

  (set-face-font 'variable-pitch j0ni/variable-font frame)
  (set-face-font 'fixed-pitch j0ni/fixed-font frame)
  (set-face-font 'fixed-pitch-serif j0ni/fixed-font-serif frame)
  (set-face-font 'mode-line j0ni/fixed-font frame)

  (set-fontset-font t 'unicode "Symbola" frame 'prepend)
  ;; if the font is paying attention ¯\_(ツ)_/¯
  (set-face-attribute
   'default frame
   :weight 'semi-light)
  (set-face-attribute
   'bold frame
   :weight 'semi-bold)

  (menu-bar-mode -1)

  (fringe-mode 8)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)

  (setq mouse-autoselect-window nil)
  (setq focus-follows-mouse nil)

  (setq x-underline-at-descent-line t)
  (setq x-use-underline-position-properties nil))

;; This is an attempt to prevent the emacsclient frame from ignoring all this
;; stuff. Unfortunately it does not appear to work. ¯\_(ツ)_/¯

;; default value
;; (setq after-make-frame-functions '(select-frame exwm-init))

(add-to-list 'after-make-frame-functions #'j0ni/init-frame)
(setq initial-frame-alist `((font . ,j0ni/fixed-font) (fullscreen . maximized)))
(setq default-frame-alist `((font . ,j0ni/fixed-font) (height . 100) (width . 120)))
(add-hook 'emacs-startup-hook #'j0ni/init-frame)

(provide 'early-init)
;;; early-init.el ends here
