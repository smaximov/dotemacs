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

(req-package yaml-path
  :disabled
  :load-path "lib"
  :require yaml-mode
  :bind (:map yaml-mode-map
              ("C-c C-p" . yaml-path/path)))

(provide 'init-yaml)
;;; init-yaml.el ends here
