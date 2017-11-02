;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     octave
     (c-c++ :variables c-c++-enable-clang-support t)
     haskell
     (latex :variables latex-enable-auto-fill nil)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables spell-checking-enable-by-default nil)
     (auto-completion :variables auto-completion-enable-help-tooltip t)
     better-defaults
     clojure
     csv
     emacs-lisp
     ess
     git
     ivy
     javascript
     markdown
     org
     python
     syntax-checking
     version-control
     )


   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(moe-dark monokai)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Dina"
                               :size 10
                               :weight normal
                               :width normal
                               :powerline-scale 1.5)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 10
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'any
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil ))

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

  (defun add-all-list (l)
    "Add bindings to global map."
    (mapcar
     (lambda (pair)
       (list 'global-set-key (list 'kbd (car pair)) (cadr pair)))
     (pair-list l)))

  (defmacro imap (&rest l)
    "Map the bindings of L with 'evil-insert-state-map."
    `(progn ,@(add-map-list 'evil-insert-state-map l)))
  (defmacro nmap (&rest l)
    "Map the bindings of L with 'evil-normal-state-map."
    `(progn ,@(add-map-list 'evil-normal-state-map l)))
  (defmacro amap (&rest l)
    "Map the bindings of L with all the maps."
    `(progn ,@(add-all-list l))))

(defun dotspacemacs/user-init ()
  (setq custom-file "~/.emacs.d/private/custom.el")
  (if (file-exists-p custom-file) (load custom-file))

  (setq vc-follow-symlinks t)
  (defvar org-confirm-babel-evaluate nil)
  (setq org-startup-indented t)
  )

(defun myhook-python ()
  (modify-syntax-entry ?_ "w"))
(defun myhook-lisp ()
  (dolist (k '(?- ?_))
    (modify-syntax-entry k "w")))
(defun myhook-haskell ()
  (setq evil-auto-indent nil)
  (setq haskell-stylish-on-save t)
  (push 'company-ghci company-backends))
(defun attach-hooks ()
  (mapc (lambda (p)
          (add-hook (car p) (cadr p) t))
        '((python-mode-hook myhook-python)
          (emacs-lisp-mode-hook myhook-lisp)
          (haskell-mode-hook myhook-haskell))))

(defun dotspacemacs/user-config ()
  (setq disabled-command-function nil)
  (setq powerline-default-separator 'arrow)
  (setq ivy-initial-inputs-alist nil)

  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq show-paren-style 'expression)
  (setq show-paren-delay 0)
  (show-paren-mode +1)
  (show-smartparens-global-mode -1)

  (amap "§" 'lisp-state-toggle-lisp-state)

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
    (interactive)
    (choose-font-if-exists "Dina"))
  (defun font-scp ()
    (interactive)
    (choose-font-if-exists "Source Code Pro"))

  (attach-hooks)
  )
