(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(global-linum-mode 1)
(show-paren-mode 1)

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
      make-backup-files nil
      )
(set-default 'truncate-lines t)
(set-default 'show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "pink")

;; font
(set-face-attribute 'default nil :font global-font-face)
(set-frame-font global-font-face nil t)

(provide 'look-and-feel)
