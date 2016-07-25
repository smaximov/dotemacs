;;; init-paredit.el --- `paredit-mode' configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package paredit
  :require dash eldoc
  :init
  (--each '(emacs-lisp-mode-hook eval-expression-minibuffer-setup-hook ielm-mode-hook
                                 lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook)
    (add-hook it #'enable-paredit-mode))
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(provide 'init-paredit)
;;; init-paredit.el ends here
