;;; init-elixir.el --- Erlang, Elixir & Alchemist -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package elixir-mode
  :ensure t
  :pin melpa-stable)

(req-package alchemist
  :ensure t
  :pin melpa-stable
  :require elixir-mode)

(req-package flycheck-credo
  :ensure t
  :pin melpa
  :require flycheck validate
  :init
  (flycheck-credo-setup)
  :config
  (validate-setq flycheck-elixir-credo-strict t))

(provide 'init-elixir)
;;; init-elixir.el ends here
