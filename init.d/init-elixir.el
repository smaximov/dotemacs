;;; init-elixir.el --- Erlang, Elixir & Alchemist -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package elixir-mode)

(req-package alchemist
  :require elixir-mode)

(req-package flycheck-credo
  :require flycheck validate
  :ensure t
  :init
  (flycheck-credo-setup)
  :config
  (validate-setq flycheck-elixir-credo-strict t))

(provide 'init-elixir)
;;; init-elixir.el ends here
