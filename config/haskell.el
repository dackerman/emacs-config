(use-package haskell-mode
  :config
  (defun my-haskell-hook ()
    (defun my-save-hook ()
      (haskell-align-imports)
      (haskell-sort-imports)
      (delete-trailing-whitespace))
    (intero-mode)
    (add-hook 'before-save-hook 'my-save-hook))

  (add-hook 'haskell-mode-hook 'my-haskell-hook))

(provide 'haskell)
