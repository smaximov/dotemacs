;;; init-sql.el --- SQL utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package sqlup-mode
  :ensure t
  :pin melpa-stable
  :hook (sql-mode sql-interactive-mode))

(provide 'init-sql)
;;; init-sql.el ends here
