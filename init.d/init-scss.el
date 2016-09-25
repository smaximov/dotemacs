;;; init-scss.el --- `scss-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package scss-mode
  :require whitespace validate
  :init
  (add-hook 'scss-mode-hook #'whitespace-mode)
  (add-hook 'scss-mode-hook (lambda ()
                              (setf comment-start "//"
                                    comment-end "")))
  :config
  (validate-setq require-final-newline t))

(provide 'init-scss)
;;; init-scss.el ends here
