;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     clojure
     javascript
     markdown
     csv
     python
     ivy
     auto-completion
     better-defaults
     emacs-lisp
     git
     ess
     (latex :variables latex-enable-auto-fill nil)
     org
     markdown
     (spell-checking :variables spell-checking-enable-by-default nil)
     (shell :variables shell-default-height 30
            shell-default-position 'bottom)
     syntax-checking
     version-control
     )


   dotspacemacs-addiflattertional-packages '()
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
   dotspacemacs-max-rollback-slots 5 dotspacemacs-helm-resize nil
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
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil ))


(defun dotspacemacs/user-init ()
  (setq custom-file "~/.emacs.d/private/custom.el")
  (if (file-exists-p custom-file) (load custom-file))
  )

(defun dotspacemacs/user-config ()
  (setq ivy-initial-inputs-alist nil)
  (setq powerline-default-separator 'arrow)
  (setq disabled-command-function nil)
  (global-set-key (kbd "§") 'lisp-state-toggle-lisp-state)

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

  )
