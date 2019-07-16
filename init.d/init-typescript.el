;;; init-typescript.el --- TypeScript support -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package typescript-mode
  :mode "\\.tsx$"
  :ensure t)

(req-package tide
  :ensure t
  :require typescript-mode flycheck
  :preface
  (defun nameless:setup-tide-mode ()
    (tide-setup)
    (tide-hl-identifier-mode)
    (add-hook 'before-save-hook #'tide-format-before-save)
    (when-let* ((tslint (nameless:locate-tslint)))
      (setq-local flycheck-typescript-tslint-executable tslint)))

  (defun nameless:locate-tslint ()
    (when-let* ((root (locate-dominating-file
                       (or (buffer-file-name) default-directory)
                       "node_modules"))
                (tslint (expand-file-name "node_modules/.bin/tslint" root)))
      (and (file-executable-p tslint) tslint)))
  :hook (typescript-mode . nameless:setup-tide-mode))

(provide 'init-typescript)
;;; init-typescript.el ends here
