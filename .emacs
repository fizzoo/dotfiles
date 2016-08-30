;;; Package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-list '(moe-theme
                     powerline powerline-evil 
                     evil evil-leader evil-matchit
                     helm magit paredit
                     smart-tab
                     slime undo-tree
                     elpy pyvenv))
(mapc #'package-install package-list)

;;; Remove ugly gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-default-font "Dina")
(setq inhibit-startup-screen t)

;;; Theme
(require 'powerline)
(require 'powerline-evil)
(require 'moe-theme)
(moe-dark)
(powerline-moe-theme)                   ;This is a magic order/combination
(powerline-evil-center-color-theme)     ; that simply werkz

;;; Evil
(require 'evil)
(setq evil-insert-state-map (make-sparse-keymap)) ; pure emacs in insert
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
(evil-define-command ERC ()
  "edit rc"
  (find-file "~/.emacs"))

(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(define-key evil-normal-state-map (kbd "q") nil)
(define-key evil-normal-state-map (kbd "C-q") 'evil-record-macro)
(evil-leader/set-key
  "q" 'delete-window
  "r" (lambda () (interactive) (evil-execute-macro 1 last-kbd-macro)))

(global-evil-leader-mode)
(evil-mode 1)
(global-evil-matchit-mode)

;;; Enable all functions
(setq disabled-command-function nil)

;;; Slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
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

;;; Tab complete when reasonable
(require 'smart-tab)
(global-smart-tab-mode 1)

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
(require 'paredit)
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

;;; Helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h") 'helm-apropos)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action) ; old tab
(helm-mode 1)

;;; VA usability binds
(global-set-key (kbd "<f1>") 'delete-other-windows)
(global-set-key (kbd "<f2>") 'helm-mini)
(global-set-key (kbd "<f3>") 'helm-find-files)

;;; esc quits: http://stackoverflow.com/questions/8483182/evil-mode-best-practice
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
