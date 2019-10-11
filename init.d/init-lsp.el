;;; init-lsp.el --- Emacs client for Language Server Protocol servers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package lsp-mode
  :ensure t
  :config
  (setf lsp-prefer-flymake nil))

(req-package company-lsp
  :ensure t
  :require company
  :config
  (push 'company-lsp company-backends))

(req-package lsp-ui
  :ensure t
  :require lsp-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here
