;;; init-editorconfig.el --- configure `editorconfig' -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package editorconfig
  :ensure t
  :pin melpa-stable
  :require diminish
  :diminish editorconfig-mode
  :hook
  (after-init . editorconfig-mode))

(provide 'init-editorconfig)
;;; init-editorconfig.el ends here
