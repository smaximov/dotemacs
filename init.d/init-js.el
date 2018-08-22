;;; init-js.el --- configure JS mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package json-mode
  :ensure t
  :hook ((json-mode . electric-pair-local-mode))
  :pin melpa-stable
  :config
  (setf js-indent-level 2))

(req-package company-tern
  :ensure t
  :pin melpa-stable
  :disabled
  :require company
  :config
  (add-to-list 'company-backends 'company-tern))

(req-package tern
  :ensure t
  :pin melpa-stable
  :disabled
  :diminish tern-mode
  :hook ((js-mode . tern-mode)
         (js-mode . eldoc-mode)))

(req-package rjsx-mode
  :ensure t
  :hook ((rjsx-mode . electric-pair-local-mode))
  :pin melpa-stable
  :require validate
  :mode "\\.js$"
  :config
  (validate-setq js2-strict-missing-semi-warning nil
                 js-indent-level 2))

(req-package vue-mode
  :ensure t
  :pin melpa-stable)

(provide 'init-js)
;;; init-js.el ends here
