;;; init-elixir.el --- Erlang, Elixir & Alchemist -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package erlang
  :ensure t
  :pin melpa-stable)

(req-package elixir-mode
  :ensure t
  :pin melpa
  :require exec-path-from-shell projectile validate
  :preface
  (defun set-default-directory-to-mix-project-root (original-fun &rest args)
    (if-let* ((mix-project-root (and (projectile-project-p)
                                     (projectile-locate-dominating-file buffer-file-name
                                                                        ".formatter.exs"))))
        (let ((default-directory mix-project-root))
          (apply original-fun args))
      (apply original-fun args)))
  :hook ((elixir-mode . (lambda () (add-hook 'before-save-hook #'elixir-format nil t))))
  :init
  (advice-add 'elixir-format :around #'set-default-directory-to-mix-project-root)
  :config
  (validate-setq elixir-format-elixir-path (executable-find "elixir")
                 elixir-format-mix-path (executable-find "mix")))

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
