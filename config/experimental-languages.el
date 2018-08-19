(use-package rust-mode
  :defer t
  :mode "\\.rs\\'")

(use-package purescript-mode
  :defer t
  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(provide 'experimental-languages)
