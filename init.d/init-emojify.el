;;; init-emojify.el --- Display emoji in emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package emojify
  :ensure t
  :pin melpa-stable
  :disabled
  :hook (after-init . global-emojify-mode))

(provide 'init-emojify)
;;; init-emojify.el ends here
