;;; init-haskell.el --- `haskell-mode' configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package haskell-mode
  :ensure t
  :require validate
  :init
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  :config
  (validate-setq haskell-font-lock-symbols t))

(req-package ebal
  :disabled
  :require validate
  :config
  (validate-setq ebal-operation-mode 'stack))

(req-package flycheck-haskell
  :disabled
  :require flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(provide 'init-haskell)
;;; init-haskell.el ends here
