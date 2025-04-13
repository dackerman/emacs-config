;;; home-nixos-desktop.el --- NixOS desktop configuration -*- lexical-binding: t -*-

;; Run emacs server
(server-start)

;; Use more memory
(setq gc-cons-threshold (* 100 1024 1024))

 ;;; Package Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun package-setup ()
  (setq default-directory "~/.emacs.d/")
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

  ;(setq straight-use-package-by-default 't)
  (bootstrap-straight-el)
  ;; No longer using use-package
  )

(defun init-deprecated-package-el ()
  (setq package-enable-at-startup nil)
  (setq package-user-dir "~/.emacs.d/packages")
  (add-to-list 'load-path "~/.emacs.d/config")

  (require 'package)
  (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
  (package-initialize))

(defun bootstrap-straight-el ()
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(package-setup)

(add-to-list 'load-path "~/.emacs.d/lisp")

(defvar custom-font "Hack-12")
(defvar enabled-features
      '(look-and-feel
        editor-features
        email
        org
        llms
        flycheck
        lsp
        clojure
        go
        emacs-lisp
        javascript
        nixos
        markdown))

(defvar enabled-features '(look-and-feel))

(load "~/.emacs.d/lisp/features")
