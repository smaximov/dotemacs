;;; init-k8s.el --- k8s support -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package kubernetes
  :ensure t
  :bind (("C-c C-k" . kubernetes-overview)))

(provide 'init-k8s)
;;; init-k8s.el ends here
