(defconst matt-default-font "Inconsolata-12")
(add-to-list 'default-frame-alist (cons 'font matt-default-font))
(set-face-font 'default matt-default-font)

;; setopts
(setq org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "firefox %s")
     ("\\.pdf\\'" . default)))
(setq shell-file-name "/usr/bin/zsh")
(setq x-super-keysym 'meta)
