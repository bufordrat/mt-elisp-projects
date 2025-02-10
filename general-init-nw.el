;; cd to homedir
(cd "~")

;; keybindings
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c f") #'mt-change-font-family)
(global-set-key (kbd "C-c s") #'mt-change-font-size)
(global-set-key (kbd "C-c m") 'magit-clone)
(global-set-key (kbd "M-+") #'scroll-up-line)
(global-set-key (kbd "M-_") #'scroll-down-line)

;; function calls
(winner-mode 1)
(windmove-default-keybindings)
(size-indication-mode 1)
(display-time)
(show-paren-mode 1)

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
(setq default-major-mode 'fundamental-mode)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; fix ansi escape nonsense while compiling
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(when (require 'ansi-color nil t)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; completion
(with-demoted-errors "%S"
  (selectrum-mode +1)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1)
  (marginalia-mode +1))

;; message mode
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-service 587)

;; bbdb
(with-eval-after-load 'bbdb
  (bbdb-initialize 'gnus 'message 'anniv))

;; shell mode zsh goodies
(add-hook 'shell-mode-hook
          (lambda ()
            (setq comint-input-ring-file-name "~/.zsh_history")
            (comint-read-input-ring t)
            (setq-local comint-output-filter-functions
                        '(comint-truncate-buffer
                          ansi-color-process-output
                          comint-postoutput-scroll-to-bottom
                          comint-watch-for-password-prompt))
            (setq-local comint-process-echoes t)))

;; org mode
(add-hook 'org-mode-hook 'org-indent-mode)
(require 'org-tempo)

;; python
(add-hook 'python-mode-hook 'eglot-ensure)
(defvar python-mode-map)
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "M-n") 
    'flymake-goto-next-error)
  (define-key python-mode-map (kbd "M-p")
    'flymake-goto-prev-error))

;; ocaml
(when (executable-find "opam")
  (let ((opam-share (car (with-demoted-errors "%S" (process-lines "opam" "var" "share"))))
        (opam-bin   (car (with-demoted-errors "%S" (process-lines "opam" "var" "bin")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share)))
    (when (and opam-bin (file-directory-p opam-bin))
      (add-to-list 'exec-path opam-bin))))

(autoload 'merlin-mode "merlin" nil t nil)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

(advice-add 'make-comint :around #'my-utop-workaround)

(defun my-utop-workaround (orig-fun name &rest args)
  (if (not (equal name "OCaml"))
      (apply orig-fun name args)
    (let ((process-connection-type nil))
      (apply orig-fun name args))))

;; haskell
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-to-list 'exec-path (expand-file-name "~/.ghcup/bin/"))

;; tint
(dolist
    (f '(run-tint tint-mode tint-eval tint-eval-at-point))
  (autoload f "tint" nil t))

;; dired
(with-demoted-errors "%s" (diredfl-global-mode +1))

(setq all-the-icons-dired-monochrome nil)
(setq backup-directory-alist '(("." . "~/.squiggles")))
(setq bbdb-file "~/bbdb/bbdb")
(setq blink-cursor-mode nil)
(setq browse-url-browser-function 'eww-browse-url)
(setq comint-buffer-maximum-size 65336)
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)
(setq diff-switches "-u")
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq enable-recursive-minibuffers t)
(setq epg-pinentry-mode 'loopback)
(setq magit-clone-set-remote.pushDefault t)
(setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
(setq proced-enable-color-flag t)
(setq ring-bell-function 'ignore)
(setq tooltip-mode nil)
(setq truncate-lines t)
(setq tuareg-opam-insinuate t)
