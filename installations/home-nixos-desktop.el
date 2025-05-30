;;; home-nixos-desktop.el --- NixOS desktop configuration -*- lexical-binding: t -*-

;; Run emacs server
(server-start)

;; Use more memory
(setq gc-cons-threshold (* 100 1024 1024))

(add-to-list 'load-path "~/.emacs.d/lisp")

(defvar custom-font "Hack-12")

(require 'features)

;; Call feature functions directly
(look-and-feel)
(editor-features)
(email)
(org)
(llms)
(flycheck)
(clojure)
(go)
(emacs-lisp)
(javascript)
(typescript)
(python)
(markdown)
(lsp)
