;;; Package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-list '(moe-theme
                     powerline powerline-evil 
                     evil evil-matchit
                     company
                     ivy counsel swiper
                     magit
                     paredit
                     slime
                     undo-tree
                     elpy pyvenv
                     org
                     ))
(mapc #'package-install package-list)
(mapc #'require package-list)

;;; Remove ugly gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-default-font "Dina")
(setq inhibit-startup-screen t)

;;; Theme
(moe-dark)
(powerline-moe-theme)                   ;This is a magic order/combination
(powerline-evil-center-color-theme)     ; that simply werkz

;;; Evil
(setq evil-insert-state-map (make-sparse-keymap)) ; pure emacs in insert
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
(evil-define-command ERC ()
  "edit rc"
  (find-file "~/.emacs"))

(evil-mode 1)
(global-evil-matchit-mode)

;;; Enable all functions
(setq disabled-command-function nil)

;;; Slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(slime-setup)

;;; Sane backup/autosave
(setq backup-directory-alist
      (list (cons ".*" (concat temporary-file-directory "backup/"))))
(setq auto-save-default nil)

;;; Persistent history
(setq undo-tree-history-directory-alist
      (list (cons "." (concat temporary-file-directory "undo/"))))
(setq undo-tree-auto-save-history t)

;;; Use spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)                      ;two of them

;;; Python
(elpy-enable)

;;; Show matching parens
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(show-paren-mode 1)

;;; Smooth scrolling
(setq scroll-margin 800)                ;maxes somewhere around 25%
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;;; Paredit, on all lisp
(autoload 'enable-paredit-mode
  "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)

;;; No emacs-vc
(setq vc-handled-backends ())
(global-set-key (kbd "C-x g") 'magit-status)

;;; Org
(org-babel-do-load-languages 'org-babel-load-languages '((C . t) (haskell . t) (emacs-lisp . t) (latex . t)))
(setq org-confirm-babel-evaluate nil)

;;; Company
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)

;;; Ivy, counsel, swiper
(ivy-mode t)
(setq ivy-use-virtual-buffers t)
(global-set-key (kbd "M-x")  'counsel-M-x)
(global-set-key "\C-s" 'swiper)

;;; VA usability bind
(global-set-key (kbd "<f1>") 'delete-other-windows)
(global-set-key (kbd "<f2>") 'ivy-switch-buffer)
(global-set-key (kbd "<f3>") 'counsel-find-file)

;;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 512)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

