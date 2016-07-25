;;; init-scss.el --- `scss-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package scss-mode
  :require whitespace
  :preface
  ;; Get rid of "assignment to free variable `css-indent-offset'" warning
  (defvar css-indent-offset)
  :init
  (add-hook 'scss-mode-hook #'whitespace-mode)
  (add-hook 'scss-mode-hook (lambda ()
                              (setf comment-start "//"
                                    comment-end "")))
  :config
  (setf css-indent-offset 2)
  (setf require-final-newline t))

(provide 'init-scss)
;;; init-scss.el ends here
