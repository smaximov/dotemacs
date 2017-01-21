;;; init-magit.el --- `magit' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package magit
  :require validate
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch-popup))
  :if (version<= "24.4" emacs-version)
  :init
  (add-hook 'after-init-hook #'global-magit-file-mode)
  :config
  (validate-setq git-commit-summary-max-length 100
                 magit-completing-read-function #'ivy-completing-read))

(req-package magithub
  :require magit
  :config (magithub-feature-autoinject t))

(provide 'init-magit)
;;; init-magit.el ends here
