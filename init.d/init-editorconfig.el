;;; init-editorconfig.el --- configure `editorconfig' -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package editorconfig
  :init
  (add-hook 'after-init-hook #'editorconfig-mode))

(provide 'init-editorconfig)
;;; init-editorconfig.el ends here
