;; emacs customisation file

;; Define package repos
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Taking this line from clojure for the brave and true setup
;; I think its necessary for the packages to be loaded or something
(package-initialize)

;; make a list of packages to install (if not already installed)
;; can also manually install with M-x package-install
(defvar my-packages
  '(
    ;; high contrast zebburn theme
    zenburn-theme
    ;; helm
    helm
    ;; paredit
    paredit
    ;; colored brackets
    rainbow-delimiters
    ;; general - used like evil leader for kdb bindings
    general
    ;; which-key shows partial keybindings
    which-key
    ))

;; now iterate over the list of packages and install if needed
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; load hc-zenburn theme
(load-theme 'zenburn t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    ;;
;; helm config                        ;;
;;                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the next two lines are the minimum for basic helm setup
(require 'helm-config)
(helm-mode 1)

;; 

;; rebind basic keys to use helm commands
(global-set-key (kbd "M-x") 'undefined)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
;; rebind esc to actually exit stuff
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    ;;
;; end of helm config                 ;;
;;                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable line numbers
(global-linum-mode t)

;; set frame (window) size
;; (add-to-list 'default-frame-alist '(height . 40))
;; (add-to-list 'default-frame-alist '(width . 140))
(setq initial-frame-alist '((top . 20) (left . 10) (width . 140) (height . 40)))

;; disable menubar toolbar & scrollbar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; highlights matching parens
(show-paren-mode 1)

;; highlight current line
(global-hl-line-mode 1)

;; don't use hard tabs
(setq-default indent-tabs-mode nil)

;; when opening a file the cursor will return to the previous location
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; set shortcuts for next/previous buffer
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-iso-lefttab>") 'previous-buffer)

;; paredit stuff
(autoload 'enable-paredit-mode "paredit" "Turn on psuedo-structural editing of Lisp code" t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

;; don't show native os scrollbars for buffers as they are redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; evil leader shortcut keys
;; should be enabled before evil mode
;; (global-evil-leader-mode)
;; (evil-leader/set-leader "<SPC>")
;; (evil-leader/set-key
;;   "f" 'helm-find-files
;;   "x" 'helm-M-x)

;; evil mode
;; must set C-u to be scroll up before the (require 'evil) line for some reason
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; general
;; used for more convienient keybindings
;; as well as multi-key shorcuts
(require 'general)
(setq gen-leader "SPC")
;; vim-like definitions, from general.el git page
(general-evil-setup)
;; all keyword arguments are still supported
;; some helm and other general shortcuts
(general-nmap :prefix gen-leader
              "p" 'helm-mini
              "f f" 'helm-find-files
              "f s" 'save-buffer
              "x" 'helm-M-x
              "b b" 'helm-buffers-list
              "b k" 'kill-buffer
              "q s" 'save-buffers-kill-terminal)
;; shortcuts for switching windows
(general-nmap :prefix gen-leader
              "TAB" 'other-window)
(general-nmap "J" 'windmove-down
              "K" 'windmove-up
              "H" 'windmove-left
              "L" 'windmove-right)
(general-nmap :prefix gen-leader
              "w d" 'delete-window
              "w o" 'delete-other-windows
              "w h" 'split-window-below
              "w v" 'split-window-right)

;; which key
(require 'which-key)
(which-key-mode)
