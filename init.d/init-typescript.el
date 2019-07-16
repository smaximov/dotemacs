;;; init-typescript.el --- TypeScript support -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package typescript-mode
  :mode "\\.tsx$"
  :ensure t)

(provide 'init-typescript)
;;; init-typescript.el ends here
