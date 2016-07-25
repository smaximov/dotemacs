;;; init-paren.el --- `paren' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package paren
  :init
  (add-hook 'after-init-hook #'show-paren-mode)
  :config
  (setf show-paren-delay 0
        show-paren-style 'mixed))

(provide 'init-paren)
;;; init-paren.el ends here
