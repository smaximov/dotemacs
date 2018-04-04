;;; init-emr.el --- configure `emr' (emacs refactoring system) -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package emr
  :ensure t
  :pin melpa-stable
  :hook (prog-mode . emr-initialize)
  :bind ([M-return] . emr-show-refactor-menu))

(provide 'init-emr)
;;; init-emr.el ends here
