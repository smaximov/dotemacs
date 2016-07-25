;;; init-yaml.el --- configure `yaml-mode' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package yaml-mode
  :mode "\\.yml$")

(provide 'init-yaml)
;;; init-yaml.el ends here
