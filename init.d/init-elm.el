;;; init-elm.el --- a function language for the Web -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package elm-mode
  :require company
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends #'company-elm))

(provide 'init-elm)
;;; init-elm.el ends here
