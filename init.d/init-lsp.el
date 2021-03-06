;;; init-lsp.el --- Emacs client for Language Server Protocol servers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package lsp-mode
  :ensure t
  :require company
  :config
  (setf lsp-prefer-flymake nil)
  (push 'company-capf company-backends))

(req-package lsp-ui
  :ensure t
  :require lsp-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :bind
  (([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
   ([remap xref-find-references] . #'lsp-ui-peek-find-references)))

(provide 'init-lsp)
;;; init-lsp.el ends here
