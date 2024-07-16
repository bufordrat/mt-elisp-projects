;; font stuff
(defconst matt-default-font "M+ 1mn-14")
(add-to-list 'default-frame-alist (cons 'font matt-default-font))
(set-face-font 'default matt-default-font)

;; exec path from shell
(exec-path-from-shell-initialize)

