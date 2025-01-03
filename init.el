;; Run emacs server
(server-start)

;; Use more memory
(setq gc-cons-threshold (* 100 1024 1024))

;;; Package Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-enable-at-startup nil)
(setq default-directory "~/.emacs.d/")
(setq package-user-dir "~/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/config")

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

;;; Platform Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-platform-config (platform)
  (cond
   ((eq platform 'linux)
    '((global-font-face . "Hack-12")) ; main font face
    )))
(load "~/.emacs.d/config/machine-config.el")
(set 'platform-config (run-platform-config machine-platform))

(global-set-key (kbd "s-q") 'fill-paragraph)

(defun dack-cider-eval-last-sexp-and-replace-formatted ()
  (interactive)
  (cider-eval-last-sexp-and-replace)
  (cider-format-edn-last-sexp))

(global-set-key (kbd "C-c C-v f") 'dack-cider-eval-last-sexp-and-replace-formatted)

(defun dack-open-cnp ()
  (interactive)
  (find-file "~/code/cnp/src/main/cnp/app.cljs")
  (cider-connect-cljs '(:host "localhost"
                              :port "37695"
                              :project-dir "~/code/cnp"
                              :cljs-repl-type shadow-select)))

(global-set-key (kbd "C-c C-d c") 'dack-open-cnp)

;;; Look and Feel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun look-and-feel ()
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode t)
  (global-display-line-numbers-mode 1)
  (show-paren-mode 1)
  (global-unset-key (kbd "C-z"))
  (setq ring-bell-function 'ignore)

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
  (set-face-attribute 'default nil :height 150)
  )

(defun set-autofill-hook ()
  (if (string-equal "notes" (projectile-project-name))
      (progn
        (auto-fill-mode 't)
        (set-fill-column 80))))

(add-hook 'markdown-mode-hook 'set-autofill-hook)

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

(defun make-save-commit ()
  (interactive)
  (projectile-run-shell-command-in-root "git add .")
  (shell-command "git commit -m 'save'")
  (shell-command "git push"))

(global-set-key (kbd "M-C-S") 'make-save-commit)

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

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

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

  (use-package treemacs)

  (use-package treemacs-projectile)

  (use-package ace-window
    :config
    (global-set-key (kbd "M-o") 'ace-window))

  (use-package helm
    :bind (("M-x" . helm-M-x)
           ("C-x C-f" . helm-find-files))
    :config
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

  (use-package keychain-environment
    :config
    (keychain-refresh-environment))

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
  (use-package flycheck-clj-kondo)

  (use-package clojure-mode
    :ensure t
    :init
    (add-hook 'clojure-mode-hook #'enable-paredit-mode)
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook 'lsp)
    (add-hook 'clojurescript-mode-hook 'lsp)
    (add-hook 'clojurec-mode-hook 'lsp)
    (add-hook 'before-save-hook 'cider-format-buffer t t)

    :config
    (require 'flycheck-clj-kondo)
    (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)))

(defun emacs-lisp ()
  (add-hook 'lisp-mode-hook #'enable-paredit-mode))

(defun common-lisp ()
  (use-package slime)
  (setq inferior-lisp-program "sbcl"))

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

(defun llms ()
  (use-package ellama
    :init
    (setopt ellama-language "English")
    (require 'llm-ollama)
    (setopt ellama-provider
	    (make-llm-ollama
	     :chat-model "mixtral" :embedding-model "mixtral"))))

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

(defun flycheck ()
  (use-package flycheck))

(defun lsp ()
  (use-package lsp-mode)
  (use-package lsp-ui)

  (lsp-treemacs-sync-mode 1))

;; Selection of features. Comment out a section to prevent it from running
(defun initialize-user-config ()
  (require 'use-package)
  (look-and-feel)
  (editor-features)
  (org-mode-settings)
  (load-theme 'dracula t)
  (flycheck)
  (lsp)
  (clojure)
  (emacs-lisp)
  (common-lisp)
  (javascript)
  (nixos)
  ;(haskell)
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
  "Opens my Emacs configuration."
  (interactive)
  (switch-to-buffer
   (find-file-noselect
    "/home/david/.emacs.d/init.el")))

(defun super-kill-line ()
  "Kill the rest of the line AND any whitespace on the next line.
This can be really useful when you want to delete an arg in a
multi-line function, and want to get rid of any space between the
point and the next paren/brace."
  (interactive)
  (kill-line)
  (delete-horizontal-space))

(global-set-key (kbd "M-k") 'super-kill-line)


(defun make-tag-rule (rule)
  "Tags a given email under point (or the current region) with the given tag.
Additionally, adds it to the tags file as a new RULE that runs
when email comes in."
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
  "Run an arbitrary cleave COMMAND."
  (interactive "scleave: ")
  (shell-command (format "/home/david/bin/cleave %s" command)))


(defun sync-mail ()
  "Syncs mail from the backend."
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
 '(cider-clojure-cli-aliases ":dev")
 '(cider-doc-auto-select-buffer nil)
 '(cider-infer-remote-nrepl-ports t)
 '(cider-print-options '(("print-length" 50)))
 '(cider-repl-pop-to-buffer-on-connect nil)
 '(custom-safe-themes
   '("73803d7cebbc240fd6cd8a54077b8fbf0b263a25db48579f5953279986283481" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" default))
 '(exec-path-from-shell-variables '("PATH" "MANPATH" "OPENAI_API_KEY"))
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
 '(nrepl-force-ssh-for-remote-hosts t)
 '(nrepl-use-ssh-fallback-for-remote-hosts t)
 '(org-agenda-files '("~/code/cnp/TODO.org"))
 '(package-selected-packages
   '(request prettier cuda-mode ellama html-to-hiccup slime multiple-cursors keychain-environment treemacs-projectile nord-theme lsp-dart dart-mode zig-mode paredit flycheck-clj-kondo company flycheck lsp-ui lsp-mode glsl-mode shader-mode notmuch ace-window markdown-mode nix-mode rainbow-delimiters cider typescript-mode yaml-mode rjsx-mode web-mode exec-path-from-shell purescript-mode rust-mode intero haskell-mode helm-projectile helm projectile fzf magit dracula-theme darktooth-theme use-package))
 '(projectile-indexing-method 'hybrid)
 '(projectile-project-search-path '("~/code"))
 '(rmail-primary-inbox-list '("maildir:///home/david/mail/gmail/Inbox"))
 '(safe-local-variable-values
   '((eval
      (lambda nil
        (defun cider-jack-in-wrapper-function
            (orig-fun &rest args)
          (if
              (and
               (boundp 'use-bb-dev)
               use-bb-dev)
              (message "Use `bb dev` to start the development server, then `cider-connect` to the port it specifies.")
            (apply orig-fun args)))
        (advice-add 'cider-jack-in :around #'cider-jack-in-wrapper-function)
        (when
            (not
             (featurep 'clerk))
          (let
              ((init-file-path
                (expand-file-name "clerk.el" default-directory)))
            (when
                (file-exists-p init-file-path)
              (load init-file-path)
              (require 'clerk))))))
     (use-bb-dev . t)
     (prettify-symbols-mode)
     (cider-shadow-cljs-default-options . "app")
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))
 '(send-mail-function 'smtpmail-send-it)
 '(shr-color-visible-luminance-min 70)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))


(defun shell-command-to-buffer (command-list input)
  "Execute COMMAND with INPUT and stream output to current buffer."
  (let* ((process (make-process
                   :name "shell-command-process"
                   :buffer (current-buffer)
                   :command command-list

                   :filter
                   (lambda (proc output)
                     (message "filtering %s" output)
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-max))
                       (insert output))))))))


(defun pipe-content-to-process (process content)
  (process-send-string process (concat cnotent "\n"))
  (process-send-eof process))

;(shell-command-to-buffer (list "sh" "-ic" "~/bin/llm") "hi\nther\ndummy")
