;; cd to homedir
(cd "~")

;; Turn off mouse interface early in startup to avoid momentary redisplay
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;turn off menu bar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;turn off tool bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;turn off vertical scroll bar
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1)) ;turn off horiz scroll bar

;; systems
;; on Linux, super is Meta
(cond
  ((eq 'gnu/linux system-type)
   (setq x-super-keysym 'meta)))

