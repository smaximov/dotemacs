;;; init-clojure.el --- `clojure' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package clojure-mode
  :require eldoc paredit
  :init
  (--each (list #'enable-paredit-mode #'eldoc-mode)
    (add-hook 'clojure-mode-hook it)))

(req-package cider
  :require eldoc paredit f validate
  :init
  (--each (list #'enable-paredit-mode #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook it))
  :config
  (validate-setq cider-lein-command (f-full "~/bin/lein")))

(provide 'init-clojure)
;;; init-clojure.el ends here
