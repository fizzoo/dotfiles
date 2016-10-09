;;; init --- Summary:
;;; Commentary:
;;; Code:

;;; No backup/autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

;;; custom-set-variables
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file) (load custom-file))

;;; Remove ugly gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;;; Font selection
(defun font-exists-p (font)
  "Check if FONT exists."
  (not (null (x-list-fonts font))))
(defun choose-font-if-exists (font)
  "Set frame font to FONT if it exists."
  (if (and (display-graphic-p) (font-exists-p font))
      (set-frame-font font)
    (message "Didn't find font %s." font)))
(choose-font-if-exists "Dina")

;;; Enable all functions
(setq disabled-command-function nil)

;;; Use spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)                      ;two of them

;;; Show matching parens
(defvar show-paren-delay 0)
(defvar show-paren-style 'expression)
(show-paren-mode 1)

;;; Smooth scrolling
(setq scroll-margin 4)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;;; No emacs-vc
(setq vc-handled-backends ())

;;; gdb
(defvar gdb-many-windows t)
(defvar gdb-show-main t)

;;; VA binds
(global-set-key (kbd "<f1>") 'delete-other-windows)

(defun zsh-terminal ()
  "Start up a terminal buffer with zsh somewhere visible.
If there already exists one that we made, use that one.
And if we're inside said buffer, start up a new zsh."
  (interactive)
  (cond ((equal (buffer-name) "*terminal*")
         (make-term "terminal" "/bin/zsh"))
        (t
         (if (get-buffer "*terminal*")
             (display-buffer "*terminal*")
           (term "/bin/zsh")))))
(global-set-key (kbd "<f4>") 'zsh-terminal)


;;; 
;;; Start of package-based settings
;;; 

;;; Package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(if (not (file-exists-p "~/.emacs.d/elpa/")) (package-refresh-contents))

;;; use-package
(if (not (featurep 'use-package)) (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t)

(use-package powerline
  :demand)
(use-package powerline-evil
  :after powerline)
(use-package moe-theme
  :after powerline-evil
  :config (progn
            (moe-dark)
            (powerline-moe-theme)
            (powerline-evil-center-color-theme)))

(use-package undo-tree
  :demand
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package evil
  :demand
  :config (progn
            (evil-define-command ERC ()
              "edit rc"
              (find-file "~/.emacs"))
            (evil-mode 1)))

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode))

(use-package slime
  :init (defvar inferior-lisp-program "/usr/bin/sbcl")
  :config (slime-setup))

(use-package elpy
  :commands elpy-enable
  :init (add-hook 'python-mode-hook 'elpy-enable))

(use-package paredit
  :commands enable-paredit-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook                  'enable-paredit-mode)
          (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
          (add-hook 'ielm-mode-hook                        'enable-paredit-mode)
          (add-hook 'lisp-mode-hook                        'enable-paredit-mode)
          (add-hook 'lisp-interaction-mode-hook            'enable-paredit-mode)
          (add-hook 'scheme-mode-hook                      'enable-paredit-mode)))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package org
  :init (progn
          (defvar org-startup-indented t)
          (defvar org-src-fontify-natively t)
          (defvar org-confirm-babel-evaluate nil))
  :config (org-babel-do-load-languages
           'org-babel-load-languages
           '((C . t) (haskell . t) (emacs-lisp . t) (latex . t) (python . t))))


(use-package company
  :demand
  :init (defvar company-idle-delay 0)
  :config (global-company-mode)
  :diminish company-mode)

(use-package irony
  :commands irony-mode
  :init (progn
          (add-hook 'c++-mode-hook 'irony-mode)
          (add-hook 'c-mode-hook 'irony-mode)
          (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package company-irony
  :after irony)
(use-package company-irony-c-headers
  :after irony
  :config (add-to-list 'company-backends '(company-irony-c-headers company-irony)))

(use-package flycheck
  :demand                               ;Pretty much in every mode, might as well
  :config (global-flycheck-mode))

(use-package flycheck-irony
  :after irony
  :config (flycheck-irony-setup))

(use-package cmake-ide
  :after irony
  :init (defvar cmake-ide-build-dir "build"))

(use-package ivy
  :after flx
  :bind ("<f2>" . ivy-switch-buffer)
  :config (progn
            (ivy-mode t)
            (setq ivy-use-virtual-buffers t)
            (setq ivy-initial-inputs-alist '()))
  :diminish ivy-mode)

(use-package flx
  :demand)

(use-package counsel
  :after ivy
  :bind ("<f3>" . counsel-find-file)
  :config (progn
            (counsel-mode 1)
            (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
  :diminish counsel-mode)

(use-package swiper
  :bind ("C-s" . swiper))

(use-package recentf
  :demand
  :config (progn
            (recentf-mode 1)
            (setq recentf-max-saved-items 512)))

(use-package tex
  :ensure auctex
  :config (setq TeX-parse-self t))

(use-package yasnippet
  :demand
  :config (progn
            (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
            (yas-global-mode))
  :diminish yas-minor-mode)

(provide 'init)
;;; .emacs ends here
