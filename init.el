;;; init.el -- Jack Costigans init.el file for GNU Emacs

;;; Commentary:
;; Emacs customisation file

;;; Code:

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
    ;; tomorrow themes
    ;; color-theme-sanityinc-tomorrow
    ;; helm
    helm
    ;; paredit
    paredit
    ;; colored brackets
    rainbow-delimiters
    ;; vim emulation
    ;; evil-mode
    ;; general - used like evil leader for kdb bindings
    general
    ;; which-key shows partial keybindings
    which-key
    ;; golden ration automatically resizes windows
    golden-ratio
    ;; flycheck is a generalised syntax checking thingy
    ;; flycheck
    ;; haskell mode
    haskell-mode
    ;; clojure mode
    clojure-mode
    ;; CIDER
    cider
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

;; rebind basic keys to use helm commands
(global-set-key (kbd "M-x") 'undefined)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
;; rebind esc to actually exit stuff
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; rebind save and search to be more like other programs
(global-set-key (kbd "C-s") 'undefined)
(global-set-key (kbd "C-s") 'save-buffer)
;; ctrl-f keys have to be set after evil loads
;; ctrl-f in vim seems stupid anyway
(defun set-ctrl-f-shortcuts ()
  (global-set-key (kbd "C-f") 'undefined)
  (global-set-key (kbd "C-f") 'isearch-forward)
  (define-key isearch-mode-map "\C-f" 'isearch-repeat-forward))
(set-ctrl-f-shortcuts)
(require 'evil-states)
(require 'evil-commands)
(eval-after-load "evil-maps"
  (define-key evil-motion-state-map "\C-f" nil))
(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
                 evil-insert-state-map
                 evil-emacs-state-map))
    (define-key (eval map) "\C-f" nil)))
(eval-after-load "evil-maps"
  (set-ctrl-f-shortcuts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    ;;
;; end of helm config                 ;;
;;                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable line numbers
(global-linum-mode t)

;; set frame (window) size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 140))
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 140) (height . 40)))

;; disable menubar toolbar & scrollbar
;; (menu-bar-mode -1)
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

;; set c indent style to linux
;; set tab offset to 4 (linus is crazy, 8 is way too much)
(setq c-default-style "linux"
      c-basic-offset 4)

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
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)


;; golden ration will automatically resize the active window
;; (golden-ratio-mode 1)

;; show whitespace
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-global-modes '(haskell-mode))

;; enabled flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; enable eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(require 'cider)
(require 'clojure-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)

;; evil mode
;; must set C-u to be scroll up before the (require 'evil) line for some reason
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

(require 'powerline)
(powerline-center-evil-theme)

;; general
;; used for more convienient keybindings
;; as well as multi-key shorcuts
(require 'general)
;; use space as leader key during normal mode
;; and C-SPC at other times
;; unbind C-SPC from set mark, visual mode should make marks mostly useless
(global-set-key (kbd "C-SPC") 'undefined)
(setq gen-leader "SPC")
(setq gen-leader-non-normal "C-SPC")
(setq repl-leader (concat gen-leader " r"))
(setq compile-leader (concat gen-leader " c"))
(setq general-default-prefix "SPC")
(setq general-default-global-prefix "C-SPC")
;; vim-like definitions, from general.el git page
(general-evil-setup)
;; all keyword arguments are still supported
;; some helm and other general shortcuts
(general-define-key :states '(normal insert emacs)
                    "p" 'helm-mini
                    "f f" 'helm-find-files
                    "f s" 'save-buffer
                    "f w" 'write-file
                    "x" 'helm-M-x
                    "b b" 'helm-buffers-list
                    "b k" 'kill-buffer
                    "b d" 'kill-this-buffer
                    "q s" 'save-buffers-kill-terminal)
;; shortcuts for switching windows
(defun prev-window ()
  "Move to the previous window.
Just calls other window with a negative number."
  (interactive) (other-window -1))
(general-define-key :states '(normal insert emacs)
                    "TAB" 'other-window
                    "<backtab>" 'prev-window)
(general-define-key :states '(normal emacs)
                    "J" 'windmove-down
                    "K" 'windmove-up
                    "H" 'windmove-left
                    "L" 'windmove-right)
(general-define-key :states '(normal insert emacs)
                    "w d" 'delete-window
                    "w o" 'delete-other-windows
                    "w h" 'split-window-below
                    "w v" 'split-window-right)
(general-define-key :states '(normal insert emacs)
                    "e b" 'flycheck-buffer
                    "e c" 'flycheck-clear
                    "e e" 'list-flycheck-errors
                    "e n" 'flycheck-next-error
                    "e p" 'flycheck-previous-error
                    "e 0" 'flycheck-first-error)

(general-define-key :states '(normal)
                    "s f" 'paredit-forward-slurp-sexp
                    "s b" 'paredit-backward-slurp-sexp)
;; which key
(require 'which-key)
(which-key-mode)

;; haskell stuff
;; use stack instead of cabal
(setq haskell-compile-cabal-build-command "stack build")
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; haskell keyboard shorcuts
;; shorcuts for repl
(general-define-key :keymaps '(haskell-mode-map interactive-haskell-mode-map haskell-cabal-mode-map inferior-haskell-mode-map)
                    :states '(normal)
                    :prefix gen-leader
                    "r l" 'haskell-process-load-file
                    "r c" 'haskell-interactive-mode-clear
                    "r s" 'haskell-interactive-bring)

;; the ones from spacemacs are
;; they all seem to be prefixed by SPC m
;; SPC m g g go to definition or tag
;; SPC m g i cycle Haskell import line?
;; SPC m f   format buffer
;; docs bidings
;; SPC m h d find or generate Haddock docs
;; SPC m h f do a helm-hoogle lookup
;; SPC m h h do a hoogle lookup
;; debug is prefixed by SPC m d
;; REPL
;; SPC m s b load or reload current buffer into REPL
;; SPC m s c clear REPL
;; SPC m s s show the REPL without switching to it
;; SPC m s s show and switch to REPL
;; intero repl?
;; Cabal commands
;; SPC m c a cabal actions
;; SPC m c b build the current cabal project, cabal/stack build
;;

;; Clojure kbd shortcuts
(general-define-key :keymaps '(clojure-mode-map cider-mode-map)
                    :states '(normal)
                    :prefix gen-leader
                    "r j" 'cider-jack-in
                    "r c" 'cider-connect
                    "r b" 'cider-load-buffer
                    "r e" 'cider-eval-last-sexp
                    "r n" 'cider-repl-set-ns
                    "r r" 'cider-switch-to-repl-buffer
                    "r d" 'cider-doc
                    "r q" 'cider-quit
                    "s l f" 'clojure-let-forward-slurp-sexp
                    "s l b" 'clojure-let-backward-slurp-sexp)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-inject-dependencies-at-jack-in nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
