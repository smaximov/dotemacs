;;; init-projectile.el --- configure `projectile' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

;; https://github.com/bbatsov/projectile/issues/991#issuecomment-248026667
(setf projectile-keymap-prefix (kbd "C-x p"))

(req-package projectile
  :require ivy
  :init
  (add-hook 'after-init-hook #'projectile-global-mode)
  :config
  (setf projectile-completion-system 'ivy-completing-read))

(provide 'init-projectile)
;;; init-projectile.el ends here
