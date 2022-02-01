(setq gc-cons-threshold 20000000)

(server-start)

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
  (add-to-list 'default-frame-alist `(font . ,(alist-get 'global-font-face platform-config)))

  (list 'font "SF Mono-14")
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

  (use-package ace-window
    :config
    (global-set-key (kbd "M-o") 'ace-window))

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
    :config (setq magit-save-repository-buffers 'dontask))

  (autoload 'notmuch "notmuch" "notmuch mail" t)
  )


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

;; Custom Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rename-file-and-buffer (new-name)
  (interactive "FRename to:")
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

(defun super-kill-line ()
  "Kills the rest of the line AND any whitespace on the next line. This can be
really useful when you want to delete an arg in a multi-line function, and want
to get rid of any space between the point and the next paren/brace."
  (interactive)
  (kill-line)
  (delete-horizontal-space))

(global-set-key (kbd "M-k") 'super-kill-line)


(defun make-tag-rule (rule)
  "Tags a given email under point (or the current region) with the given tag.
Additionally, adds it to the tags file as a new rule that runs when email comes
in."
  (interactive
   ;; Custom interactive section lets us pre-populate
   ;; the query before the user edits it.
   (let* ((addr (if (use-region-p)
                    (buffer-substring start end)
                  (thing-at-point 'email)))
          (initial-rule (format "{:add [\"tag\"] :query [:from \"%s\"]}" addr))
          (rule (read-string "Rule: " initial-rule)))

     ;; return the args in order that add-from-rule takes them
     (list rule)))

  (shell-command
   (format
    "/home/david/bin/cleave add-rule '/home/david/code/notmuch-tags/notmuch-tags.edn' '%s'"
    rule)))


(defun sync-mail ()
  (interactive)
  (ansi-term "/home/david/bin/sync-mail" "sync-mail-status"))


(defun handle-in-google-chrome (handle)
  (mm-display-external handle "google-chrome-stable %s"))


(defun open-mime-in-browser ()
  "Opens the mime-type under point (generally text/html) in chrome"
  (interactive)
  (notmuch-show-apply-to-current-part-handle #'handle-in-google-chrome))


(add-hook 'notmuch-show-mode-hook
          (lambda ()
            (define-key notmuch-show-mode-map "\.v" 'open-mime-in-browser)))


;;; Custom Set Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "multimarkdown")
 '(notmuch-archive-tags '("-inbox"))
 '(notmuch-hello-tag-list-make-query "tag:unread")
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "(tag:unread and tag:inbox)" :key "u")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "emacs-devel" :query "tag:forums/emacs and tag:inbox" :key "e")))
 '(notmuch-search-oldest-first nil)
 '(package-selected-packages
   '(notmuch ace-window markdown-mode nix-mode rainbow-delimiters cider typescript-mode yaml-mode rjsx-mode web-mode exec-path-from-shell purescript-mode rust-mode intero haskell-mode helm-projectile helm projectile fzf magit dracula-theme darktooth-theme use-package))
 '(rmail-primary-inbox-list '("maildir:///home/david/mail/gmail/Inbox"))
 '(safe-local-variable-values
   '((intero-targets "mailroom-server:lib" "mailroom-server:exe:mailroom-server" "mailroom-server:exe:mailroom-worker" "mailroom-server:test:test")
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))
 '(shr-color-visible-luminance-min 70))
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
