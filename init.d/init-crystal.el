;;; init-crystal.el --- Crystal language support -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package crystal-mode
  :ensure t
  :pin melpa
  :hook ((crystal-mode . (lambda ()
                           (add-hook 'before-save-hook #'crystal-tool-format nil t))))
  :bind (:map crystal-mode-map
              ("C-c , a" . crystal-spec-all)
              ("C-c , v" . crystal-spec-buffer)
              ("C-c , s" . crystal-spec-line)
              ("C-c , t" . crystal-spec-switch)))

(req-package flycheck-crystal
  :ensure t
  :require crystal-mode flycheck
  :pin melpa)

(provide 'init-crystal)
;;; init-crystal.el ends here
