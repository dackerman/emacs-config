(setq gc-cons-threshold 20000000)

;; Initialize package system
(setq package-enable-at-startup nil)
(setq default-directory "~/.emacs.d/")
(setq package-user-dir "~/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/config")

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)

(require 'machine-config)

(require 'look-and-feel)

(require 'editor-features)

(require 'experimental-languages)

(require 'haskell)

(require 'ruby)

(load-theme 'dracula t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell purescript-mode rust-mode intero haskell-mode helm-projectile helm projectile fzf magit dracula-theme darktooth-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
