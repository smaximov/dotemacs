;;; init-autoinsert.el --- configure and load `autoinsert'  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package autoinsert
  :hook (prog-mode . auto-insert-mode)
  :init
  ;; Emacs package header skeleton
  (define-auto-insert '(emacs-lisp-mode . "Emacs Package Skeleton")
    '("Short description: "
      ";;; " (file-name-nondirectory buffer-file-name) " --- " str " -*- lexical-binding: t -*-" ?\n ?\n
      ";;; Commentary:" ?\n ?\n
      ";;; Code:" ?\n ?\n
      _ ?\n ?\n
      "(provide '" (file-name-base) ")" ?\n
      ";;; " (file-name-nondirectory buffer-file-name) " ends here")))

(provide 'init-autoinsert)
;;; init-autoinsert.el ends here
