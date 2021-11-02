;;; init-paredit.el --- `paredit-mode' configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package paredit
  :ensure t
  :pin melpa-stable
  :require eldoc diminish
  :diminish paredit-mode
  :hook
  ((emacs-lisp-mode eval-expression-minibuffer-setup ielm-mode lisp-mode lisp-interaction-mode scheme-mode)
   . enable-paredit-mode)
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(provide 'init-paredit)
;;; init-paredit.el ends here
