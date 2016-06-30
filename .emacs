;;; Package manager
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;;; Remove ugly gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-default-font "Dina")

;;; Theme
(require 'powerline)
(require 'moe-theme)
(moe-theme-set-color 'blue)
(moe-dark)
(powerline-moe-theme)

;;; Evil
(require 'evil)
(evil-mode 1)
(setq evil-insert-state-map (make-sparse-keymap)) ; pure emacs in insert
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
(evil-define-command ERC ()
  "edit rc"
  (find-file "~/.emacs"))

;;; Slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup)

;;; Sane backup/autosave dir
(setq backup-directory-alist
      (list (cons ".*" (concat temporary-file-directory "backup/"))))
(setq auto-save-file-name-transforms
      (list (list ".*" (concat temporary-file-directory "autosave/") t)))

;;; Persistent history
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      (list (cons "." (concat temporary-file-directory "undo/"))))

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
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action) ; old tab
(helm-mode 1)

;;; VA usability binds
(global-set-key (kbd "<f1>") 'delete-other-windows)
(global-set-key (kbd "<f2>") 'helm-mini)
(global-set-key (kbd "<f3>") 'helm-find-files)
(define-key evil-normal-state-map " "
  (lambda () (interactive) (evil-execute-macro 1 last-kbd-macro)))

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
