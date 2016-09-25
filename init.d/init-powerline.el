;;; init-powerline.el --- powerline setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package powerline
  :require validate
  :config
  (validate-setq powerline-gui-use-vcs-glyph t)
  (powerline-default-theme))

(provide 'init-powerline)
;;; init-powerline.el ends here
