;;; init-julia.el --- julia language -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package julia-mode)

(req-package julia-shell
  :disabled
  :require julia-mode
  :mode ("\\.jl$" . julia-mode)
  :bind (:map julia-mode-map
              ("C-x C-e" . julia-shell-run-region-or-line)
              ("C-c C-l" . julia-shell-save-and-go)
              ("C-c C-s" . run-julia)))

(provide 'init-julia)
;;; init-julia.el ends here
