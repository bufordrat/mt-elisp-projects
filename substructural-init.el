;; font stuff
(defconst matt-default-font "M+ 1mn-14")
(add-to-list 'default-frame-alist (cons 'font matt-default-font))
(set-face-font 'default matt-default-font)

;; haskell lsp
(add-hook 'haskell-mode-hook 'eglot-ensure)

;; setopts
(setopt display-battery-mode t)
(setopt org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "open %s")
     ("\\.pdf\\'" . default)))
(setopt shell-file-name "/bin/zsh")
