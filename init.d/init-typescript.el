;;; init-typescript.el --- TypeScript support -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package typescript-mode
  :mode "\\.tsx$"
  :ensure t)

(req-package tide
  :ensure t
  :require typescript-mode
  :preface
  (defun nameless:setup-tide-mode ()
    (tide-setup)
    (tide-hl-identifier-mode)
    (add-hook 'before-save-hook #'tide-format-before-save))
  :hook (typescript-mode . nameless:setup-tide-mode))

(provide 'init-typescript)
;;; init-typescript.el ends here
