;;; init-projectile.el --- configure `projectile' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package projectile
  :init
  (add-hook 'after-init-hook #'projectile-global-mode))

(provide 'init-projectile)
;;; init-projectile.el ends here
