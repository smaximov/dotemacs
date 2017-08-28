;;; init-emr.el --- configure `emr' (emacs refactoring system) -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package emr
  :disabled
  :init
  (add-hook 'prog-mode-hook #'emr-initialize)
  :bind ([M-return] . emr-show-refactor-menu))

(provide 'init-emr)
;;; init-emr.el ends here
