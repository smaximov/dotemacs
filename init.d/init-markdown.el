;;; init-markdown.el --- configure `markdown-mode' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package markdown-mode
  :mode ("\\.md$" "\\.markdown$")
  :config
  (setf markdown-command "kramdown"))

(provide 'init-markdown)
;;; init-markdown.el ends here
