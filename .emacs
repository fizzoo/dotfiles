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
(choose-font-if-exists "Dina 13")

;;; Enable all functions
(setq disabled-command-function nil)

;;; Use spaces
(setq-default indent-tabs-mode nil)

;;; Truncate if off screen
(setq-default truncate-lines t)

;;; Show matching parens
(defvar show-paren-delay 0)
(defvar show-paren-style 'expression)
(show-paren-mode 1)

;;; Smooth scrolling
(setq scroll-conservatively 8)

;;; No emacs-vc
(setq vc-handled-backends ())

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

;;; Start of use-package stanzas
;; I will somewhat keep to a system of placing options and necessary
;; hooks in the :init and :config fields, but defining most useful
;; keybindings further down this file.
;; :bind is easily replaced with a :command and map in init, which is
;; also more in line with how the command+hook works.

(use-package powerline)
(use-package powerline-evil)
(use-package moe-theme
  :config
  (progn
    (moe-dark)
    (powerline-moe-theme)
    (powerline-evil-center-color-theme)))

;; System for defining keys easily, relies on evil since i use that.
(eval-when-compile
  (defun pair-list (l)
    "Pair elements in even list L."
    (if l
        (cons (list (car l) (cadr l))
              (pair-list (cddr l)))
      nil))
  (defun add-map (map pair)
    "Create a 'define-key to MAP with mapping PAIR."
    (list 'define-key map (list 'kbd (car pair)) (cadr pair)))
  (defun add-map-list (map l)
    "Perform add-map using MAP on all of list L."
    (mapcar (lambda (x) (add-map map x)) (pair-list l)))
  (defmacro imap (&rest l)
    "Map the bindings of L with 'evil-insert-state-map."
    `(progn ,@(add-map-list 'evil-insert-state-map l)))
  (defmacro nmap (&rest l)
    "Map the bindings of L with 'evil-normal-state-map."
    `(progn ,@(add-map-list 'evil-normal-state-map l)))
  (defmacro amap (&rest l)
    "Map the bindings of L with all the maps."
    `(progn ,@(add-map-list 'evil-normal-state-map l)
            ,@(add-map-list 'evil-insert-state-map l)
            ,@(add-map-list 'evil-emacs-state-map l))))

(use-package evil
  :config
  (progn
    (evil-define-command ERC ()
      "edit rc"
      (find-file "~/.emacs"))
    (evil-mode 1)))

(use-package evil-matchit
  :config (global-evil-matchit-mode))

(use-package which-key
  :config
  (progn
    (which-key-mode)
    (setq which-key-idle-delay 0.2))
  :diminish which-key-mode)

(use-package undo-tree
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package smartparens
  :config (progn
            (require 'smartparens-config)
            (smartparens-global-mode)))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package magit)

(use-package flx)

(use-package ivy
  :config
  (progn
    (ivy-mode t)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist '()))
  :diminish ivy-mode)

(use-package counsel
  :config
  (progn
    (counsel-mode 1)
    (setq ivy-re-builders-alist '((swiper . ivy--regex-plus) (t . ivy--regex-fuzzy))))
  :diminish counsel-mode)

(use-package swiper)

(use-package recentf
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-saved-items 512)))

(use-package company
  :init (defvar company-idle-delay 0.1)
  :config (global-company-mode)
  :diminish company-mode)

;; http://emacs.stackexchange.com/a/24800
(dolist (key '("<return>" "RET"))
  (define-key company-active-map (kbd key)
    `(menu-item nil company-complete
                :filter ,(lambda (cmd)
                            (when (company-explicit-action-p)
                            cmd)))))
(define-key company-active-map (kbd "TAB") #'company-complete-selection)
(define-key company-active-map (kbd "SPC") nil)
(setq company-auto-complete-chars nil)

(use-package flycheck
  :config (progn (setq flycheck-check-syntax-automatically '(save mode-enabled))))

(use-package yasnippet
  :config
  (progn
    (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode))
  :diminish yas-minor-mode)

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

;; Markdown
(use-package markdown-mode)
(use-package flymd
  :defer)

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

(use-package cmake-ide
  :after irony
  :init (defvar cmake-ide-build-dir "build"))

;; TeX
(use-package tex
  :defer
  :ensure auctex
  :config (setq TeX-parse-self t))

;; Haskell
(use-package haskell-mode
  :init (add-hook 'haskell-mode-hook
                  (lambda () (interactive)
                    (setq evil-auto-indent nil)
                    (interactive-haskell-mode)))
  :config (setq haskell-stylish-on-save t))

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



;;; Global bindings area

;; Inherit most emacs bindings instead of the few vim
;; insert-mode ones
(setq evil-insert-state-map (make-sparse-keymap))

;; For some reason "<escape>" and "ESC" differ here, it
;; undoes all meta keys otherwise
(amap "<escape>" 'evil-normal-state)

(nmap "SPC 1" 'delete-other-windows
      "SPC 2" 'ivy-switch-buffer
      "SPC 3" 'counsel-find-file
      "SPC 4" 'zsh-terminal)

(defun flycheck-mode-and-check ()
  (interactive)
  (flycheck-mode 'toggle)
  (flycheck-buffer))
(nmap "SPC 5" 'flycheck-mode-and-check)

(amap
 "C-<right>" 'sp-forward-sexp
 "C-<left>" 'sp-backward-sexp
 "C-<up>" 'sp-up-sexp
 "C-<down>" 'sp-down-sexp
 "C-S-<right>" 'sp-forward-slurp-sexp
 "C-S-<left>" 'sp-forward-barf-sexp
 "C-S-<up>" 'sp-backward-slurp-sexp
 "C-S-<down>" 'sp-backward-barf-sexp
 "C-M-t" 'sp-transpose-sexp
 "C-M-w" 'sp-unwrap-sexp)


(nmap "SPC SPC" 'evil-emacs-state)
(nmap "SPC q" 'evil-delete-buffer)
(nmap "SPC g" 'magit-status)
(amap "C-s" 'swiper)

(setq python-shell-completion-native-enable nil)

(provide 'init)
;;; .emacs ends here
