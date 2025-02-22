;; Run emacs server
(server-start)

;; Use more memory
(setq gc-cons-threshold (* 100 1024 1024))

(add-to-list 'load-path "~/.emacs.d/lisp")

(defvar custom-font "Hack-12")
(defvar enabled-features
      '(look-and-feel
        editor-features
        email
        org
        flycheck
        lsp
        clojure
        emacs-lisp
        javascript
        nixos
        markdown))

(require 'features)
