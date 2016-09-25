;;; init-scss.el --- `scss-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package scss-mode
  :require whitespace validate
  :init
  (require 'css-mode)
  (add-hook 'scss-mode-hook #'whitespace-mode)
  (add-hook 'scss-mode-hook (lambda ()
                              (setf comment-start "//"
                                    comment-end "")))
  :config
  (validate-setq require-final-newline t
                 css-indent-offset 2))

(provide 'init-scss)
;;; init-scss.el ends here
