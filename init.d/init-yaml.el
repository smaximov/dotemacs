;;; init-yaml.el --- configure `yaml-mode' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package yaml-mode
  :mode "\\.yml$")

(req-package yaml-path :loader :path :force t
  :load-path "lib"
  :require yaml-mode
  :bind (:map yaml-mode-map
              ("C-c C-p" . yaml-path/path)))

(provide 'init-yaml)
;;; init-yaml.el ends here
