;; font stuff
(defconst matt-default-font "M+ 1mn-12")
(add-to-list 'default-frame-alist (cons 'font matt-default-font))
(set-face-font 'default matt-default-font)

;; load pdf tools
(pdf-loader-install)

;; setopts
(setopt shell-file-name "/usr/bin/zsh")
(setopt x-super-keysym 'meta)
