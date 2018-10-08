;;; init-nix.el --- Nix integration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package nix-mode
  :ensure t
  :mode "\\.nix$")

(provide 'init-nix)
;;; init-nix.el ends here
