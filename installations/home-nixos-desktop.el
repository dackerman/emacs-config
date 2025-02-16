;; Run emacs server
(server-start)

;; Use more memory
(setq gc-cons-threshold (* 100 1024 1024))

(setq emacs-root "~/.emacs.d/")

(load (concat emacs-root "modules.el"))

(package-setup)
(require 'use-package)

(look-and-feel "Hack-12")
(editor-features)
(email)
(org)
(flycheck)
(lsp)
(clojure)
(emacs-lisp)
(javascript)
(nixos)
(markdown)


;; disabled
'((haskell)
  (rust)
  (purescript)
  (ruby)
  (common-lisp)
  (typescript))
