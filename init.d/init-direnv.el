;;; init-direnv.el --- direnv integration for emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package direnv
  :ensure t
  :require exec-path-from-shell
  :init
  (direnv-mode))

(provide 'init-direnv)
;;; init-direnv.el ends here
