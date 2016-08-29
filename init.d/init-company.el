;;; init-company.el --- `company-mode' configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package company
  :require diminish
  :diminish company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :bind ([C-tab] . company-indent-or-complete-common)
  :config
  (setf company-tooltip-align-annotations t
        company-idle-delay 0.1
        company-minimum-prefix-length 3))

(provide 'init-company)
;;; init-company.el ends here
