;;; init-markdown.el --- configure `markdown-mode' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package markdown-mode
  :require validate
  :mode "\\.md$" "\\.markdown$"
  :config
  (validate-setq markdown-command (or (executable-find "pandoc")
                                      (executable-find "kramdown"))))

(provide 'init-markdown)
;;; init-markdown.el ends here
