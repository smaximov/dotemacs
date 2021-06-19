;;; init-cask.el --- `cask-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package cask-mode
  :ensure t
  :pin melpa-stable
  :require paredit
  :hook (cask-mode . enable-paredit-mode))

(req-package cask
  :disabled
  :ensure t
  :pin melpa-stable)

(provide 'init-cask)
;;; init-cask.el ends here
