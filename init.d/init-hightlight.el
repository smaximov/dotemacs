;;; init-hightlight.el --- Minor mode to highlight indentation -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package highlight-indentation
  :ensure t
  :pin melpa-stable)

(provide 'init-hightlight)
;;; init-hightlight.el ends here
