;;; init-flycheck.el --- configure `flycheck'        -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package flycheck
  :ensure t
  :require diminish direnv
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
