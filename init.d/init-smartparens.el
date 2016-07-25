;;; init-smartparens.el --- configure `smartparens' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package smartparens
  :force t
  :config
  (require 'smartparens-config))

(provide 'init-smartparens)
;;; init-smartparens.el ends here
