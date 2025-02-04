;; font stuff
(defconst matt-default-font "M+ 1mn-12")
(add-to-list 'default-frame-alist (cons 'font matt-default-font))
(set-face-font 'default matt-default-font)

;; load pdf tools
(pdf-loader-install)

;; setopts
(setopt display-battery-mode t)
(setopt org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "firefox %s")
     ("\\.pdf\\'" . default)))
(setopt shell-file-name "/usr/bin/zsh")
(setopt x-super-keysym 'meta)
