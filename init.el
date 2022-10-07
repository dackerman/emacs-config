
;; Run emacs server
(server-start)

;; Use more memory
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
  (add-to-list 'default-frame-alist `(font . ,(alist-get 'global-font-face platform-config)))

  (list 'font "SF Mono-14")
  )

(defun org-mode-settings ()
  (setq org-log-done 'time)
  (setq org-todo-keywords
        '((sequence "TODO" "DOING(!/!)" "|" "DONE"))))

;;; Editor Features ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq email-tags-file "/home/david/code/notmuch-tags/notmuch-tags.edn")

(defun notmuch-apply-address-filters ()
  (interactive)
  (let ((filter-contents (buffer-string))
        (filters-file (make-temp-file "address-filters")))
    (append-to-file filter-contents nil filters-file)
    (shell-command (format "/home/david/bin/cleave bulk-add-rules %s %s" email-tags-file filters-file))))

(defun notmuch-search-for-address (addr)
  (interactive
   (list (thing-at-point 'email)))
  (let ((query (format "from:%s" addr)))
    (notmuch-search query nil nil nil t)
                                        ;(other-window 1)
    (display-buffer
     (notmuch-search-buffer-title query)
     '(display-buffer-use-some-window . ((inhibit-same-window . t))))))


(defun edit-address-view (&optional query)
  (interactive)
  (let* ((actual-query (or query "tag:inbox and tag:uncategorized"))
         (command (format "notmuch address --format=sexp %s" actual-query))
         (addresses (car (read-from-string (shell-command-to-string command))))
         (formatted (seq-map (lambda (row) (format "unsubscribe -- %s" (plist-get row :name-addr))) addresses)))
    (kill-matching-buffers "<notmuch-address:.*" nil t)
    (switch-to-buffer (format "<notmuch-address:%s>" actual-query))
    (insert (string-join formatted "\n"))))


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

  (use-package company-mode
    :hook clojure-mode)

  (autoload 'notmuch "notmuch" "notmuch mail" t)
  (global-set-key (kbd "C-c C-a") 'notmuch-search-for-address)
  )


;;; Programming Languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rust ()
  (use-package rust-mode
    :defer t
    :mode "\\.rs\\'"))

(defun clojure ()
  (use-package cider)
  (use-package paredit
    :ensure t)
  (use-package flycheck-clj-kondo)
  (use-package clojure-mode
    :ensure t
    :init
    (add-hook 'clojure-mode-hook #'enable-paredit-mode)
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    :config
    (require 'flycheck-clj-kondo)))

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
  (use-package nix-mode)
  (use-package nix-sandbox))

(defun flycheck ()
  (use-package flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(defun lsp ()
  (use-package lsp-mode
    :hook ((c++-mode) . lsp-deferred)
    :commands lsp)
  (use-package lsp-ui
    :commands lsp-ui-mode))

;; Selection of features. Comment out a section to prevent it from running
(defun initialize-user-config ()
  (require 'use-package)
  (look-and-feel)
  (editor-features)
  (org-mode-settings)
  (load-theme 'dracula t)
  (flycheck)
  (clojure)
  (haskell)
  (javascript)
  (nixos)
  (lsp)
  ;; (rust)
  ;; (purescript)
  ;; (ruby)
  ;; (typescript)
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
                  (replace-regexp-in-string "[<>]" "" (thing-at-point 'email))))
          (initial-rule (format "{:add [\"tag\"] :query [:from \"%s\"]}" addr))
          (rule (read-string "Rule: " initial-rule)))

     ;; return the args in order that add-from-rule takes them
     (list rule)))

  ;; Shell out to my custom clojure command to apply the tags
  (shell-command
   (format
    "/home/david/bin/cleave add-rule '/home/david/code/notmuch-tags/notmuch-tags.edn' '%s'"
    rule)))


(defun cleave (command)
  "Runs an arbitrary cleave command"
  (interactive "scleave: ")
  (shell-command (format "/home/david/bin/cleave %s" command)))


(defun sync-mail ()
  (interactive)
  ;; Kill previous buffers so they don't all build up
  (kill-matching-buffers "sync-mail-status" nil 't)
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


(defun nixos-rebuild ()
  (interactive)
  (sudo-shell-command "*nixos-rebuild*" "nixos-rebuild switch"))


(defun sudo-shell-command (name command)
  (kill-matching-buffers name nil 't)
  (ansi-term
   (concat "echo " (shell-quote-argument (read-passwd "Password? "))
           (format " | sudo -S %s" command))
   name))





;;; Custom Set Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-clients-clangd-args
   '("--run" "clangd --header-insertion-decorators=0" "/home/david/code/vulkan-test/shell.nix"))
 '(lsp-clients-clangd-executable "nix-shell")
 '(markdown-command "multimarkdown")
 '(notmuch-archive-tags '("-inbox"))
 '(notmuch-hello-tag-list-make-query "tag:unread")
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "read-later" :query "(tag:unread and tag:read-later)")
     (:name "unread newsletters" :query "(tag:unread and tag:newsletters)")
     (:name "unread forums" :query "(tag:unread and tag:forums)")
     (:name "recently auto-archived" :query "(tag:unread and tag:auto-archive and -tag:unsubscribe and date:-14d..today)")
     (:name "unread" :query "(tag:unread and tag:inbox)" :key "u")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "emacs-devel" :query "tag:forums/emacs and tag:inbox" :key "e")))
 '(notmuch-search-oldest-first nil)
 '(notmuch-wash-wrap-lines-length 80)
 '(org-agenda-files '("~/code/cnp/TODO.org"))
 '(package-selected-packages
   '(lsp-dart dart-mode zig-mode paredit flycheck-clj-kondo company flycheck nix-sandbox lsp-ui lsp-mode glsl-mode shader-mode notmuch ace-window markdown-mode nix-mode rainbow-delimiters cider typescript-mode yaml-mode rjsx-mode web-mode exec-path-from-shell purescript-mode rust-mode intero haskell-mode helm-projectile helm projectile fzf magit dracula-theme darktooth-theme use-package))
 '(rmail-primary-inbox-list '("maildir:///home/david/mail/gmail/Inbox"))
 '(safe-local-variable-values
   '((intero-targets "mailroom-server:lib" "mailroom-server:exe:mailroom-server" "mailroom-server:exe:mailroom-worker" "mailroom-server:test:test")
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))
 '(send-mail-function 'smtpmail-send-it)
 '(shr-color-visible-luminance-min 70)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
