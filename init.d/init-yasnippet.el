;;; init-yasnippet.el --- `yasnippet' configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package yasnippet
  :require validate
  :init
  (add-hook 'after-init-hook #'yas-global-mode)
  :config
  (validate-setq yas-snippet-dirs `(,(expand-file-name "snippets"
                                                       user-emacs-directory))))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
