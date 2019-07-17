;;; init-typescript.el --- TypeScript support -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package add-node-modules-path
  :ensure t)

(req-package typescript-mode
  :mode "\\.tsx$"
  :require flycheck add-node-modules-path prettier-js
  :ensure t
  :hook ((typescript-mode . add-node-modules-path)
         (typescript-mode . prettier-js-mode)))

(req-package tide
  :ensure t
  :require typescript-mode
  :preface
  (defun nameless::setup-tide-mode ()
    (tide-setup)
    (tide-hl-identifier-mode))
  :hook (typescript-mode . nameless::setup-tide-mode)
  :config
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint :append))

(req-package prettier-js
  :ensure t)

(provide 'init-typescript)
;;; init-typescript.el ends here
