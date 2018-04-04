;;; init-flycheck.el --- configure `flycheck'        -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package flycheck
  :ensure t
  :pin melpa-stable
  :hook (after-init . global-flycheck-mode)
  :config
  (setf flycheck-disabled-checkers '(javascript-jshint)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
