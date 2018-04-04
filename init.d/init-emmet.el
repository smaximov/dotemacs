;;; init-emmet.el --- Zen Coding for Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package emmet-mode
  :ensure t
  :pin melpa-stable
  :hook (sgml-mode css-mode)
  :config
  (setf emmet-move-cursor-between-quotes t
        emmet-self-closing-tag-style " /"))

(provide 'init-emmet)
;;; init-emmet.el ends here
