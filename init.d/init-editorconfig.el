;;; init-editorconfig.el --- configure `editorconfig' -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package editorconfig
  :require diminish
  :diminish editorconfig-mode
  :init
  (add-hook 'after-init-hook #'editorconfig-mode))

(provide 'init-editorconfig)
;;; init-editorconfig.el ends here
