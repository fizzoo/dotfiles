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
(setq evil-insert-state-map (make-sparse-keymap)) ; pure emacs in insert
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
(evil-define-command ERC ()
  "edit rc"
  (find-file "~/.emacs"))

;;; Remove ugly gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-default-font "Dina")

;;; Theme
(require 'moe-theme)
(load-theme 'moe-dark t)

;;; Slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup)

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

;;; No emacs-vc
(setq vc-handled-backends ())
(global-set-key (kbd "C-x g") 'magit-status)

;;; Customizer vars (automatic gui settings)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6bf237d23440fb0b340f4336695f2a08c6b785aa98288b3313526e76c38bca19" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
