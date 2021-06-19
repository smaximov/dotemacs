;;; init-yasnippet.el --- `yasnippet' configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package yasnippet
  :ensure t
  :pin melpa-stable
  :require validate diminish
  :diminish yas-minor-mode
  :hook
  (after-init . yas-global-mode)
  :config
  (validate-setq yas-snippet-dirs `(,(expand-file-name "snippets"
                                                       user-emacs-directory))))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
