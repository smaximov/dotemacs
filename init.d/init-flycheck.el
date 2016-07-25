;;; init-flycheck.el --- configure `flycheck'        -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setf flycheck-disabled-checkers '(javascript-jshint)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
