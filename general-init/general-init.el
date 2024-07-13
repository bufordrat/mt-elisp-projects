;; cd to homedir
(cd "~")

;; keybindings
(global-set-key (kbd "C-+") 'text-scale-adjust) ; embiggen font
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c f") #'mt-change-font-family)
(global-set-key (kbd "C-c s") #'mt-change-font-size)
(global-set-key (kbd "C-c m") 'magit-clone)

;; function calls
(winner-mode 1)
(windmove-default-keybindings)
(size-indication-mode 1)
(display-time)
(show-paren-mode 1)
(pdf-loader-install)

(with-demoted-errors "%s"
  (diredfl-global-mode +1))
(with-demoted-errors "%S"
  (selectrum-mode +1)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1)
  (marginalia-mode +1))

;; on Linux, super is Meta
(cond
  ((eq 'gnu/linux system-type)
   (setq x-super-keysym 'meta)))

;; menu bar stuff
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; package-initialize
(setq package-enable-at-startup nil)
(package-initialize)

;; repos
(dolist (arc '(("melpa-stable" . "http://stable.melpa.org/packages/")
	       ("melpa" . "http://melpa.org/packages/")
               ("kw" . "http://www.lib.uchicago.edu/keith/software/emacs/packages/")))
  (add-to-list 'package-archives arc t))
(setq package-archive-priorities '(("kw" . 11) ("melpa-stable" . 10))) 

;; no defcustoms for these; sad
(setq default-major-mode 'fundamental-mode
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; lsps
(add-hook 'python-mode-hook 'eglot-ensure)

;; fix ansi escape nonsense while compiling
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(when (require 'ansi-color nil t)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; message mode
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-service 587)

;; bbdb
(with-eval-after-load 'bbdb
  (bbdb-initialize 'gnus 'message 'anniv))

