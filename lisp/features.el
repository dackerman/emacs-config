;;; features.el --- Modular features for Emacs configuration -*- lexical-binding: t -*-

;;; Look and Feel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun look-and-feel ()
 (straight-use-package 'dracula-theme)
 (load-theme 'dracula t)

 (menu-bar-mode -1)
 (scroll-bar-mode -1)
 (tool-bar-mode -1)
 (column-number-mode t)
 (global-display-line-numbers-mode 1)
 (show-paren-mode 1)
 (global-unset-key (kbd "C-z"))
 (global-set-key (kbd "s-q") 'fill-paragraph)
 (setq ring-bell-function 'ignore)

 (setq-default indent-tabs-mode nil)   ; tabs to spaces
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
 (when (and window-system custom-font)
   (add-to-list 'default-frame-alist `(font . ,custom-font)))

 (set-face-attribute 'default nil :height 100))

(defun org ()
 (setq org-log-done 'time)
 (setq org-todo-keywords
       '((sequence "TODO" "DOING(!/!)" "|" "DONE"))))



;;; Editor Features ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun email ()
  (autoload 'notmuch "notmuch" "notmuch mail" t)
  (setq email-tags-file "/home/david/code/notmuch-tags/notmuch-tags.edn")

  (defun open-mime-in-browser ()
    "Opens the mime-type under point (generally text/html) in chrome"
    (interactive)
    (notmuch-show-apply-to-current-part-handle #'handle-in-google-chrome))


  (add-hook 'notmuch-show-mode-hook
          (lambda ()
            (define-key notmuch-show-mode-map "\.v" 'open-mime-in-browser)))

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

  (global-set-key (kbd "C-c C-a") 'notmuch-search-for-address)

  (defun edit-address-view (&optional query)
    (interactive)
    (let* ((actual-query (or query "tag:inbox and tag:uncategorized"))
           (command (format "notmuch address --format=sexp %s" actual-query))
           (addresses (car (read-from-string (shell-command-to-string command))))
           (formatted (seq-map (lambda (row) (format "unsubscribe -- %s" (plist-get row :name-addr))) addresses)))
      (kill-matching-buffers "<notmuch-address:.*" nil t)
      (switch-to-buffer (format "<notmuch-address:%s>" actual-query))
      (insert (string-join formatted "\n"))))

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
    (ansi-term "/home/david/bin/sync-mail" "sync-mail-status")))


(defun make-save-commit ()
  (interactive)
  (projectile-run-shell-command-in-root "git add .")
  (shell-command "git commit -m 'save'")
  (shell-command "git push"))

(global-set-key (kbd "M-C-S") 'make-save-commit)


(defun editor-features ()
  (straight-use-package 'projectile)
  ;; Init
  (setq projectile-indexing-method 'alien)
  (setq projectile-use-git-grep t)
  (setq helm-projectile-fuzzy-match nil)
  (setq projectile-tags-command "/usr/local/bin/ctags -Re -f \"%s\" %s")
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;; Config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (straight-use-package 'paredit)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)

  (straight-use-package 'ace-window)
  (global-set-key (kbd "M-o") 'ace-window)

  (straight-use-package 'helm)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (helm-mode 1)

  (straight-use-package 'helm-projectile)
  (with-eval-after-load 'helm
    (with-eval-after-load 'projectile
      (helm-projectile-on)))

  (straight-use-package 'fzf)
  ;; Commented out configuration
  ;(global-set-key (kbd "C-x C-f") 'fzf-projectile)

  (straight-use-package 'magit)
  (global-set-key (kbd "C-c m s") 'magit-status)
  (setq magit-save-repository-buffers 'dontask)

  (straight-use-package 'keychain-environment)
  (keychain-refresh-environment)

  (straight-use-package 'company)
  (add-hook 'emacs-lisp-mode-hook #'company-mode)
  (add-hook 'clojure-mode-hook #'company-mode))

(defun llms ()
 (straight-use-package 'gptel)

 (setq claude
       (gptel-make-anthropic "Claude"
         :stream t
         :key (lambda () (with-temp-buffer
                           (insert-file-contents "/home/david/claude-api-key.txt")
                           (buffer-string)))))

 (setq
  gptel-model 'claude-3-7-sonnet-20250219
  gptel-backend claude))

;;; Programming Languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun markdown ()
            (defun set-autofill-hook ()
              (if (string-equal "notes" (projectile-project-name))
                  (progn
                    (auto-fill-mode 't)
                    (set-fill-column 80))))

            (add-hook 'markdown-mode-hook 'set-autofill-hook))


(defun rust ()
  (straight-use-package 'rust-mode)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(defun go ()
 (straight-use-package 'go-mode))

(defun clojure ()
            (straight-use-package 'cider)
            (straight-use-package 'flycheck-clj-kondo)

            (defun clerk-show ()
              (interactive)
              (when-let
                  ((filename (buffer-file-name)))
                (save-buffer)
                (cider-interactive-eval
                 (concat "(nextjournal.clerk/show! \"" filename "\")"))))

            (straight-use-package 'rainbow-delimiters)

            (straight-use-package 'clojure-mode)
            
            ;; Init
            (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
            (add-hook 'clojure-mode-hook 'lsp)
            (add-hook 'clojurescript-mode-hook 'lsp)
            (add-hook 'clojurec-mode-hook 'lsp)
            (add-hook 'before-save-hook 'cider-format-buffer t t)

            ;; Config
            (with-eval-after-load 'clojure-mode
              (require 'flycheck-clj-kondo)
              (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show))

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

            (global-set-key (kbd "C-c C-d c") 'dack-open-cnp))


(defun emacs-lisp ()
  )


(defun common-lisp ()
  (straight-use-package 'slime)
  (setq inferior-lisp-program "sbcl"))


(defun purescript ()
  (straight-use-package 'purescript-mode)
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))


(defun haskell ()
  (defun haskell-save-hook ()
    (message "haskell save hook running")
    (haskell-align-imports)
    (haskell-sort-imports)
    (delete-trailing-whitespace))

  (defun my-haskell-hook ()
    (message "haskell hook")
    (add-hook 'before-save-hook 'haskell-save-hook))

  (straight-use-package 'intero)
  (message "configuring intero")
  (add-hook 'haskell-mode-hook 'intero-mode)

  (straight-use-package 'haskell-mode)

  (message "setting haskell-mode-hook ")
  (add-hook 'haskell-mode-hook 'my-haskell-hook))


(defun llms-ollama ()
  (straight-use-package 'ellama)
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "mixtral" :embedding-model "mixtral")))


(defun ruby ()
  (straight-use-package 'ruby-mode)
  (defun my-ruby-mode-hook ()
    (set-fill-column 80)
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)
    (setq ruby-insert-encoding-magic-comment nil))
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))


(defun javascript ()
  (straight-use-package 'rjsx-mode)
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode)))


(defun typescript ()
  (straight-use-package 'typescript-mode))


(defun nixos ()
  (straight-use-package 'nix-mode))


(defun flycheck ()
  (straight-use-package 'flycheck))


(defun lsp ()
  (straight-use-package 'lsp-mode)
  (straight-use-package 'lsp-ui))


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


(defun handle-in-google-chrome (handle)
  (mm-display-external handle "google-chrome-stable %s"))


(defun nixos-rebuild ()
  (interactive)
  (sudo-shell-command "*nixos-rebuild*" "nixos-rebuild switch"))


(defun sudo-shell-command (name command)
  (kill-matching-buffers name nil 't)
  (ansi-term
   (concat "echo " (shell-quote-argument (read-passwd "Password? "))
           (format " | sudo -S %s" command))
   name))

(defun sync-packages ()
  (interactive)
  (package-install-selected-packages)
  (initialize-user-config)
  )

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


(provide 'features)
