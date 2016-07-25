;;; init-js.el --- configure JS mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package json-mode
  :config
  (setf js-indent-level 2))

(req-package js2-mode
  :mode "\\.js$"
  :init
  (add-hook 'js2-mode-hook #'electric-pair-mode)
  :config
  (setf js2-strict-missing-semi-warning nil
        js2-basic-offset 2
        js2-strict-trailing-comma-warning nil))

(req-package company-tern
  :require company
  :config
  (add-to-list 'company-backends 'company-tern))

(req-package tern
  :require js2-mode dash
  :diminish tern-mode
  :init
  (--each '(tern-mode eldoc-mode)
    (add-hook 'js-mode-hook it)))

(provide 'init-js)
;;; init-js.el ends here
