;;; init-projectile.el --- configure `projectile' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package projectile
  :ensure t
  :pin melpa-stable
  :require ivy validate
  :preface
  ;; https://github.com/bbatsov/projectile/issues/991#issuecomment-248026667
  (defvar projectile-keymap-prefix (kbd "C-x p"))
  :hook (after-init-hook . projectile-global-mode)
  :config
  (validate-setq projectile-completion-system 'ivy-completing-read))

(provide 'init-projectile)
;;; init-projectile.el ends here
