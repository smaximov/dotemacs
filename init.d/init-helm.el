;;; init-helm.el --- configure HELM -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package helm
  :init
  (require 'helm-config)
  (require 'helm-files)
  (require 'helm-buffers)
  (add-hook 'after-init-hook #'helm-mode)
  :bind (([remap execute-extended-command] . helm-M-x) ; M-x
         ([remap yank-pop] . helm-show-kill-ring)      ; M-y
         ([remap list-buffers] . helm-mini)            ; C-x C-b
         ([remap switch-to-buffer] . helm-mini)        ; C-x b
         ([remap find-file] . helm-find-files)         ; C-x C-f
         ([remap occur] . helm-occur)))                ; M-s o

(req-package helm-projectile
  :require projectile
  :bind ([remap projectile-switch-project] . helm-projectile-switch-project))

(req-package helm-descbinds
  :init
  (add-hook 'after-init-hook #'helm-descbinds-mode))

(req-package helm-ag
  :require helm
  :bind (([remap helm-do-grep-ag] . helm-do-ag)))

(req-package helm-describe-modes
  :bind (([remap describe-mode] . helm-describe-modes)))

(provide 'init-helm)
;;; init-helm.el ends here
