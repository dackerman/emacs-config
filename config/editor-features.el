(use-package projectile
  :init
  (setq projectile-indexing-method 'alien)
  (setq projectile-use-git-grep t)
  (setq helm-projectile-fuzzy-match nil)
  (setq projectile-tags-command "/usr/local/bin/ctags -Re -f \"%s\" %s")
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
  :config (setq magit-save-repository-buffers 'dontask))

(provide 'editor-features)
