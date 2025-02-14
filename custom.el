

;;; Custom Set Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
 '(smtpmail-smtp-service 587)
 '(zig-zig-bin "/home/david/bin/zig"))
