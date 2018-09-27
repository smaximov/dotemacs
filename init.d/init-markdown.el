;;; init-markdown.el --- configure `markdown-mode' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package markdown-mode
  :ensure t
  :pin melpa-stable
  :require validate
  :mode "\\.md$" "\\.markdown$"
  :preface
  (defun make-markdown-readable ()
    (set-face-attribute 'markdown-code-face nil :background "#37474f"))
  :hook ((after-init . make-markdown-readable))
  :config
  (validate-setq markdown-command (or (executable-find "pandoc")
                                      (executable-find "kramdown"))))

(provide 'init-markdown)
;;; init-markdown.el ends here
