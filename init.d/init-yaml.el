;;; init-yaml.el --- configure `yaml-mode' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package yaml-mode
  :mode "\\.yml$"
  :require highlight-indentation
  :init
  (add-hook 'yaml-mode-hook #'highlight-indentation-current-column-mode))

(req-package yaml-path
  :disabled
  :load-path "lib"
  :require yaml-mode
  :bind (:map yaml-mode-map
              ("C-c C-p" . yaml-path/path)))

(provide 'init-yaml)
;;; init-yaml.el ends here
