;;; init-scss.el --- `scss-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package scss-mode
  :ensure t
  :pin melpa-stable
  :require whitespace validate
  :init
  (require 'css-mode)
  (setq-default css-indent-offset 2)
  :hook
  ((scss-mode . whitespace-mode)
   (scss-mode . (lambda ()
                  (setf comment-start "//"
                        comment-end ""))))
  :config
  (validate-setq require-final-newline t))

(provide 'init-scss)
;;; init-scss.el ends here
