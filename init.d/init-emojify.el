;;; init-emojify.el --- Display emoji in emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package emojify
  :disabled
  :init
  (add-hook 'after-init-hook #'global-emojify-mode))

(provide 'init-emojify)
;;; init-emojify.el ends here
