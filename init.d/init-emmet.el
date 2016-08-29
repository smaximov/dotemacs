;;; init-emmet.el --- Zen Coding for Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package emmet-mode
  :init
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode))

(provide 'init-emmet)
;;; init-emmet.el ends here
