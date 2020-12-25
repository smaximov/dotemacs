;;; init-yaml.el --- configure `yaml-mode' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package yaml-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.yml$"
  :require highlight-indentation
  :hook (yaml-mode . highlight-indentation-current-column-mode))

(provide 'init-yaml)
;;; init-yaml.el ends here
