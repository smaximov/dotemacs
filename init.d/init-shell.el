;;; init-shell.el --- configure shell-related libraries -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(req-package sh-script :loader :built-in
  :ensure sh-script
  :mode "\\.zsh$"
  :mode "\\.zsh-theme$")

(provide 'init-shell)
;;; init-shell.el ends here
