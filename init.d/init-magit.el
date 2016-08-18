;;; init-magit.el --- `magit' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch-popup))
  :if (version<= "24.4" emacs-version)
  :init
  (add-hook 'after-init-hook #'global-magit-file-mode)
  :config
  (setf git-commit-summary-max-length 100))

(provide 'init-magit)
;;; init-magit.el ends here
