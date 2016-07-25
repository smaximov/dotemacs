;;; init-asm.el --- configure `asm-mode' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package asm-mode :loader :built-in
  :config
  (setf tab-stop-list (number-sequence 4 200 4)))

(provide 'init-asm)
;;; init-asm.el ends here
