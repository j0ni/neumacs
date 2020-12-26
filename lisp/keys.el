;;; keys.el --- Unicode shortcuts and other intersectional stuff

(defvar j0ni/unicode-mapping-alist
  '((U . "Ü")
    (u . "ü")
    (A . "Ä")
    (a . "ä")
    (O . "Ö")
    (o . "ö")
    (S . "ẞ")
    (s . "ß")
    (l . "λ")))

(defun j0ni/bind-unicode-shortcut (map ch)
  "Simplify the binding a tiny bit."
  (let ((new-ch (alist-get ch j0ni/unicode-mapping-alist)))
    (define-key map (kbd (symbol-name ch))
      `(lambda () (interactive) (insert ,new-ch)))))

(defvar j0ni/unicode-shortcut-map)
(setq j0ni/unicode-shortcut-map
      (let ((map (make-sparse-keymap)))
        (j0ni/bind-unicode-shortcut map 'U)
        (j0ni/bind-unicode-shortcut map 'u)
        (j0ni/bind-unicode-shortcut map 'A)
        (j0ni/bind-unicode-shortcut map 'a)
        (j0ni/bind-unicode-shortcut map 'O)
        (j0ni/bind-unicode-shortcut map 'o)
        (j0ni/bind-unicode-shortcut map 'S)
        (j0ni/bind-unicode-shortcut map 's)
        (j0ni/bind-unicode-shortcut map 'l)
        map))

(fset 'j0ni/unicode-shortcut-map j0ni/unicode-shortcut-map)

(global-set-key (kbd "C-'") 'j0ni/unicode-shortcut-map)

(provide 'keys)
