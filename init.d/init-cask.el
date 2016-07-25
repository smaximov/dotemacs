;;; init-cask.el --- `cask-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package cask-mode
  :require paredit
  :init
  (add-hook 'cask-mode-hook #'enable-paredit-mode))

(provide 'init-cask)
;;; init-cask.el ends here
