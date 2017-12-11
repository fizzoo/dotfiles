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
(setq use-dialog-box nil)

;;; Font selection
(defun font-exists-p (font)
  "Check if FONT exists."
  (not (null (x-list-fonts font))))
(defun choose-font-if-exists (font)
  "Set frame font to FONT if it exists."
  (interactive "sSet font to: ")
  (if (and (display-graphic-p) (font-exists-p font))
      (set-frame-font font)
    (message "Didn't find font %s." font)))
(defun font-dina ()
  "Switch font to dina."
  (interactive)
  (choose-font-if-exists "Dina"))
(defun font-scp ()
  "Switch font to source code pro."
  (interactive)
  (choose-font-if-exists "Source Code Pro"))
(font-dina)

;;; Enable all functions
(setq disabled-command-function nil)

;;; Use spaces
(setq-default indent-tabs-mode nil)

;;; Show matching parens
(defvar show-paren-delay 0)
(defvar show-paren-style 'expression)
(show-paren-mode 1)

;;; Smooth scrolling
(setq scroll-conservatively 8)

;;; No emacs-vc
;; (setq vc-handled-backends ())
(setq vc-follow-symlinks t)

;;; gdb
(defvar gdb-many-windows t)
(defvar gdb-show-main t)

;;; VA binds
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


;;;
;;; Start of package-based settings
;;;

;;; Package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(if (not (file-exists-p "~/.emacs.d/elpa/")) (package-refresh-contents))

;;; use-package
(eval-when-compile
  (unless (require 'use-package nil 'noerror)
    (package-install 'use-package)
    (require 'use-package)))
(setq use-package-always-ensure t)

(use-package diminish)

(use-package evil
  :config (progn
            (evil-mode 1)
            (add-to-list 'evil-emacs-state-modes 'debugger-mode)
            (add-hook 'debugger-mode 'evil-emacs-state)
            (add-hook 'special-mode 'evil-emacs-state)))

(use-package powerline)
(use-package powerline-evil
  :after powerline
  :config (powerline-evil-center-color-theme))
(use-package doom-themes
  :config
  (progn
    (load-theme 'doom-one t)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)))


;; System for defining keys easily, relies on evil since i use that.
(eval-when-compile
  (defun add-kbds (l)
    "Add kbd before every even element in list L."
    (cl-assert (= (mod (length l) 2) 0) t
               (format "List length of %s not divisble by 2" l))
    (if l
        (cons (list 'kbd (nth 0 l))
              (cons (nth 1 l)
                    (add-kbds (nthcdr 2 l))))))
  (defmacro imapm (map &rest l)
    "Map the bindings of L with 'evil-insert-state-map."
    `(evil-define-key 'insert ,map ,@(add-kbds l)))
  (defmacro nmapm (map &rest l)
    "Map the bindings of L with 'evil-normal-state-map."
    `(evil-define-key 'normal ,map ,@(add-kbds l)))
  (defmacro amapm (map &rest l)
    "Map the bindings of L with all the maps."
    `(evil-define-key nil ,map ,@(add-kbds l)))
  (defmacro imap (&rest l)
    `(imapm global-map ,@l))
  (defmacro nmap (&rest l)
    `(nmapm global-map ,@l))
  (defmacro amap (&rest l)
    `(amapm global-map ,@l)))

(use-package evil-matchit
  :config (global-evil-matchit-mode))

(use-package neotree
  :defer
  :init (progn
          (nmap "SPC f t" 'neotree)
          (add-to-list 'evil-emacs-state-modes 'neotree-mode)))

(use-package which-key
  :config
  (progn
    (which-key-mode)
    (setq which-key-idle-delay 0.01))
  :diminish which-key-mode)

(use-package undo-tree
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package ws-butler
  :diminish ws-butler-mode
  :config
  (progn
    (setq ws-butler-keep-whitespace-before-point nil)
    (ws-butler-global-mode t)))

(use-package aggressive-indent
  :config
  (progn
    (global-aggressive-indent-mode)
    (add-to-list 'aggressive-indent-excluded-modes
                 'haskell-mode)))

(use-package magit :defer)

(use-package ivy
  :config
  (progn
    (ivy-mode t)
    (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist '()))
  :diminish ivy-mode)
(use-package ivy-hydra)

(use-package counsel
  :init (counsel-mode t)
  :diminish counsel-mode)

(use-package swiper)

(use-package recentf
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-saved-items 512)))

(use-package company
  :init (defvar company-idle-delay 0.01)
  :config (progn
            (global-company-mode)
            (setq company-frontends
                  '(company-pseudo-tooltip-frontend
                    company-echo-metadata-frontend
                    company-preview-frontend))
            (define-key company-active-map (kbd "<return>") nil)
            (amapm company-active-map
                   "<return>" nil
                   "RET" nil
                   "C-SPC" 'company-complete-selection))
  :diminish company-mode)
(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode 1))

(use-package ggtags
  :init
  (progn
    (nmap "M-." 'ggtags-find-tag-dwim)
    (nmap "M-?" 'ggtags-find-reference)))

(use-package flycheck
  :defer
  :config (progn
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (add-to-list 'evil-emacs-state-modes 'flycheck-mode)))

(use-package yasnippet
  :config
  (progn
    (setq-default yas-snippet-dirs '("~/.snippets"))
    (yas-global-mode))
  :diminish yas-minor-mode)

(use-package projectile
  :config (projectile-mode 1))
(use-package counsel-projectile
  :after projectile
  :config (progn
            (counsel-projectile-on)
            (nmap "SPC f SPC" 'counsel-projectile)))

(use-package rg
  :config (progn
            (nmap "SPC r SPC" 'rg-project)
            (nmap "SPC r d" 'rg-dwim)
            (nmap "SPC r r" 'rg)))

;; Org
(use-package org
  :defer                                ;In default alist
  :init
  (progn
    (defvar org-startup-indented t)
    (defvar org-src-fontify-natively t)
    (defvar org-confirm-babel-evaluate nil))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t) (haskell . t) (emacs-lisp . t) (latex . t) (python . t))))
(use-package org-ref
  :after org)

;; Markdown
(use-package markdown-mode :defer)
(use-package flymd :defer)

;; Lisp
(use-package slime
  :defer
  :init (defvar inferior-lisp-program "/usr/bin/sbcl")
  :config (slime-setup))

;; Python
(use-package anaconda-mode :defer)
(use-package company-anaconda :defer)

(defun pyhook ()
  "Start my py mode."
  (interactive)
  (modify-syntax-entry ?_ "w")
  (anaconda-mode)
  (anaconda-eldoc-mode)
  (add-to-list 'company-backends '(company-anaconda)))
(add-hook 'python-mode-hook 'pyhook)

;; C/C++
(use-package irony
  :defer
  :init
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package company-irony
  :after irony
  :config (add-to-list 'company-backends '(company-irony)))
(use-package company-irony-c-headers
  :after irony
  :config (add-to-list 'company-backends '(company-irony-c-headers)))

(use-package flycheck-irony
  :after irony
  :config (flycheck-irony-setup))

(use-package cuda-mode
  :defer)
(use-package clang-format
  :after irony
  :config (nmapm 'c++-mode-map
                 "SPC a" 'clang-format-buffer))

;; TeX
(use-package tex
  :defer
  :ensure auctex
  :config (setq TeX-parse-self t))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

;; Haskell
(use-package haskell-mode
  :defer
  :init (add-hook 'haskell-mode-hook
                  (lambda () (interactive)
                    (setq evil-auto-indent nil)
                    (interactive-haskell-mode)))
  :config
  (progn
    (setq haskell-stylish-on-save t)
    (nmapm 'haskell-mode-map
           "M-?" 'haskell-mode-find-uses
           "M-." 'haskell-mode-jump-to-def-or-tag)))
(use-package company-ghc
  :after haskell-mode
  :config (add-to-list 'company-backends 'company-ghc))
(use-package company-ghci
  :after haskell-mode
  :config (add-to-list 'company-backends 'company-ghci))

;; R
(use-package ess
  :defer)

;; JS
(use-package js2-mode
  :defer
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode)))
  :config (setq js2-strict-missing-semi-warning nil
                js2-basic-offset 2))
(use-package json-mode
  :defer
  :init (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

(use-package web-mode
  :defer)




;;; Global bindings area

;; Inherit most emacs bindings instead of the few vim
;; insert-mode ones
(setq evil-insert-state-map (make-sparse-keymap))

;; For some reason "<escape>" and "ESC" differ here, it
;; undoes all meta keys otherwise
(amap "<escape>" 'evil-normal-state)

(nmap
 "SPC b b" 'ivy-switch-buffer
 "SPC f f" 'counsel-find-file
 "SPC f z" 'zsh-terminal)

(defun edit-emacs-rc ()
  "Go to emacsrc."
  (interactive)
  (find-file "~/.emacs"))
(nmap "SPC f e" 'edit-emacs-rc)

(defun flycheck-toggle-and-check ()
  "Toggle flycheck mode and check if it was previously off."
  (interactive)
  (flycheck-mode 'toggle)
  (if (bound-and-true-p flycheck-mode) (flycheck-buffer)))

(nmap "SPC t" 'flycheck-toggle-and-check)
(nmap "SPC o t" 'toggle-truncate-lines)

(nmap "SPC g" 'magit-status)
(nmap "SPC b d" 'evil-delete-buffer)
(amap "C-s" 'swiper)
(nmap "C-u" 'evil-scroll-up)
(nmap "C-w SPC" 'delete-other-windows)

(amapm highlight-uses-mode-map
       "M-n" 'highlight-uses-mode-next
       "M-p" 'highlight-uses-mode-prev)

(imap "C-SPC" 'company-complete)

(setq python-shell-completion-native-enable nil)

(defun righty-python ()
  (interactive)
  (unless (get-buffer "*Python*")
    (run-python))
  (delete-other-windows)
  (evil-window-vsplit)
  (display-buffer "*Python*"))

(nmap "SPC p" 'righty-python)

(provide 'init)
;;; .emacs ends here
