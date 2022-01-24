(setq gc-cons-threshold 20000000)


;;; Package Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun package-setup ()
  (setq package-enable-at-startup nil)
  (setq default-directory "~/.emacs.d/")
  (setq package-user-dir "~/.emacs.d/packages")
  (add-to-list 'load-path "~/.emacs.d/config")

  (require 'package)
  (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
  (package-initialize))
(package-setup)


;;; Platform Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-platform-config (platform)
  (cond
   ((eq platform 'linux)
    '((global-font-face . "Hack-12")) ; main font face
    )))
(load "~/.emacs.d/config/machine-config.el")
(set 'platform-config (run-platform-config machine-platform))




;;; Look and Feel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun look-and-feel ()
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode t)
  (global-linum-mode 1)
  (show-paren-mode 1)
  (global-unset-key (kbd "C-z"))

  (setq-default indent-tabs-mode nil) ; tabs to spaces
  (setq inhibit-startup-message t
        inhibit-startup-echo-area-message t
        tab-width 2
        scroll-step 1
        kill-whole-line t
        auto-save-timeout 10
        auto-save-file-name-transforms (progn
                                         (make-directory "~/.emacs.d/auto-save-files/" t)
                                         `((".*" "~/.emacs.d/auto-save-files/" t)))
        backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
        make-backup-files nil
        create-lockfiles nil
        )
  (set-default 'truncate-lines t)
  (set-face-background 'trailing-whitespace "pink")

  ;; font
  (set-face-attribute 'default nil :font (alist-get 'global-font-face platform-config))
  (set-frame-font (alist-get 'global-font-face platform-config) nil t)
  )

;;; Editor Features ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun editor-features ()
  (use-package projectile
    :init
    (setq projectile-indexing-method 'alien)
    (setq projectile-use-git-grep t)
    (setq helm-projectile-fuzzy-match nil)
    (setq projectile-tags-command "/usr/local/bin/ctags -Re -f \"%s\" %s")
    (add-hook 'before-save-hook 'delete-trailing-whitespace)
    :config
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    )

  (use-package helm
    :bind (("M-x" . helm-M-x))
    :config
    (require 'helm-config)
    (helm-mode 1))

  (use-package helm-projectile
    :config
    (helm-projectile-on))

  (use-package fzf
    :config
    (global-set-key (kbd "C-c C-f") 'fzf-projectile))

  (use-package magit
    :bind (("C-c m s" . magit-status))
    :config (setq magit-save-repository-buffers 'dontask)))

;;; Programming Languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rust ()
  (use-package rust-mode
    :defer t
    :mode "\\.rs\\'"))

(defun clojure ()
  (use-package cider))

(defun purescript ()
  (use-package purescript-mode
    :defer t
    :init
    (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)))

(defun haskell ()
  (defun haskell-save-hook ()
    (message "haskell save hook running")
    (haskell-align-imports)
    (haskell-sort-imports)
    (delete-trailing-whitespace))

  (defun my-haskell-hook ()
    (message "haskell hook")
    (add-hook 'before-save-hook 'haskell-save-hook))

  (use-package intero
    :config
    (message "configuring intero")
    (add-hook 'haskell-mode-hook 'intero-mode))

  (use-package haskell-mode)

  (message "setting haskell-mode-hook ")
  (add-hook 'haskell-mode-hook 'my-haskell-hook))

(defun ruby ()
  (use-package ruby-mode
    :config
    (defun my-ruby-mode-hook ()
      (set-fill-column 80)
      (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)
      (setq ruby-insert-encoding-magic-comment nil))
    (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)))

(defun javascript ()
  (use-package rjsx-mode)
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode)))

(defun typescript ()
  (use-package typescript-mode))

(defun nixos ()
  (use-package nix-mode))

;; Selection of features. Comment out a section to prevent it from running
(defun initialize-user-config ()
  (require 'use-package)
  (look-and-feel)
  (editor-features)
  (load-theme 'dracula t)
  (clojure)
  ;; (rust)
  ;; (purescript)
  ;; (haskell)
  ;; (ruby)
 (javascript)
  ;; (typescript)
  (nixos)
  )

(defun rename-file-and-buffer (new-name)
  (interactive "fRename to:")
  (rename-file (buffer-file-name) new-name)
  (kill-buffer)
  (find-file new-name))

(defun delete-file-and-buffer ()
  (interactive)
  (delete-file (buffer-file-name))
  (kill-buffer))

(defun open-nixos-config ()
  (interactive)
  (switch-to-buffer
   (find-file-noselect
    "/home/david/code/nixos-config/etc/nixos/configuration.nix")))

(defun open-emacs-config ()
  (interactive)
  (switch-to-buffer
   (find-file-noselect
    "/home/david/.emacs.d/init.el")))

;;; Custom Set Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-mode nix-mode rainbow-delimiters cider typescript-mode yaml-mode rjsx-mode web-mode exec-path-from-shell purescript-mode rust-mode intero haskell-mode helm-projectile helm projectile fzf magit dracula-theme darktooth-theme use-package))
 '(safe-local-variable-values
   '((intero-targets "mailroom-server:lib" "mailroom-server:exe:mailroom-server" "mailroom-server:exe:mailroom-worker" "mailroom-server:test:test")
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun sync-packages ()
  (interactive)
  (package-install-selected-packages)
  (initialize-user-config)
  )

; Initialize
(initialize-user-config)
