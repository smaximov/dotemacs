;;; init-typescript.el --- TypeScript support -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package typescript-mode
  :mode "\\.tsx$"
  :require flycheck
  :ensure t
  :preface
  (defun nameless::locate-node-executable (name)
    (when-let* ((root (locate-dominating-file
                       (or (buffer-file-name) default-directory)
                       "node_modules"))
                (executable (expand-file-name (concat "node_modules/.bin/" name) root)))
      (and (file-executable-p executable) executable)))

  (defun nameless::set-flycheck-eslint-executable ()
    (when-let* ((eslint (nameless::locate-node-executable "eslint")))
      (setq-local flycheck-javascript-eslint-executable eslint)))
  :hook (typescript-mode . nameless::set-flycheck-eslint-executable))

(req-package tide
  :ensure t
  :require typescript-mode
  :preface
  (defun nameless::setup-tide-mode ()
    (tide-setup)
    (tide-hl-identifier-mode)
    (add-hook 'before-save-hook #'tide-format-before-save))
  :hook (typescript-mode . nameless::setup-tide-mode)
  :config
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint :append))

(provide 'init-typescript)
;;; init-typescript.el ends here
