;;; init-iedit.el --- edit multiple regions simultaneously -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package iedit
  :ensure t
  :hook (prog-mode . iedit-mode))

(provide 'init-iedit)
;;; init-iedit.el ends here
