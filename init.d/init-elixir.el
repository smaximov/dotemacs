;;; init-elixir.el --- Erlang, Elixir & Alchemist -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package elixir-mode)

(req-package alchemist
  :require elixir-mode)

(provide 'init-elixir)
;;; init-elixir.el ends here
