;;; init-markdown.el --- configure `markdown-mode' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package markdown-mode
  :mode ("\\.md$" "\\.markdown$")
  :config
  (setf markdown-command (or (executable-find "pandoc")
                             (executable-find "kramdown"))))

(provide 'init-markdown)
;;; init-markdown.el ends here
