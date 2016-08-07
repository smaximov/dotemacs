;;; init-shell.el --- configure shell-related libraries -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package sh-script :loader :built-in
  :mode ("\\.zsh$" . sh-mode)
  :mode ("\\.zsh-theme$" . sh-mode))

(provide 'init-shell)
;;; init-shell.el ends here
