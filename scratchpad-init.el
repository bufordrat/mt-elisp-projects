;; modeline
;; (add-hook 'after-init-hook #'doom-modeline-mode)

;; dired
(require 'nerd-icons)
(require 'nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(with-demoted-errors "%s" (diredfl-global-mode +1))
