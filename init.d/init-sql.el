;;; init-sql.el --- SQL utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package sqlup-mode
  :force t
  :init
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))

(provide 'init-sql)
;;; init-sql.el ends here
