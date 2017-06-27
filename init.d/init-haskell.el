;;; init-haskell.el --- `haskell-mode' configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(req-package ebal
  :require validate
  :config
  (validate-setq ebal-operation-mode 'stack))

(req-package flycheck-haskell
  :require flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(provide 'init-haskell)
;;; init-haskell.el ends here
