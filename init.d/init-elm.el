;;; init-elm.el --- a function language for the Web -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package elm-mode
  :require company
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-hook 'elm-mode-hook #'electric-pair-local-mode)
  (add-to-list 'company-backends #'company-elm))

(req-package flycheck-elm
  :require flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(provide 'init-elm)
;;; init-elm.el ends here
