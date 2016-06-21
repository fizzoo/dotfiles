;;; Package manager
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;;; Manual scripts
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "paredit.el")

;;; Evil
(require 'evil)
(evil-mode 1)
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

;;; Remove ugly gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-default-font "Dina")

;;; Slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup)

;;; Pure emacs while in insert mode
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

;;; Sane backup/autosave dir
(setq backup-directory-alist `((".*"
				. ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*"
					,temporary-file-directory t)))

;;; Use spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)                      ;two of them

;;; Python
(elpy-enable)

;;; Tab complete when reasonable
(require 'smart-tab)
(global-smart-tab-mode 1)

;;; Paredit, on all lisp
(autoload 'enable-paredit-mode
  "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; Customizer vars (automatic gui settings)
(custom-set-variables '(inhibit-startup-screen t))
(custom-set-faces)
