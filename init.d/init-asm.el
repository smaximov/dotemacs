;;; init-asm.el --- configure `asm-mode' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package asm-mode
  :disabled
  :require validate
  :config
  (validate-setq tab-stop-list (number-sequence 4 200 4)))

(provide 'init-asm)
;;; init-asm.el ends here
