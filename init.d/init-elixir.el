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
  :require projectile lsp-mode
  :preface
  (defun set-default-directory-to-mix-project-root (original-fun &rest args)
    (if-let ((mix-project-root (and (projectile-project-p)
                                    (projectile-locate-dominating-file buffer-file-name
                                                                       "mix.exs"))))
        (let ((default-directory mix-project-root))
          (apply original-fun args))
      (apply original-fun args)))

  (defun setup-elixir-format-arguments ()
    (setq-local elixir-format-arguments
                (when-let ((formatter-dir (and (projectile-project-p)
                                               (projectile-locate-dominating-file
                                                buffer-file-name
                                                ".formatter.exs"))))
                  `("--dot-formatter" ,(concat formatter-dir ".formatter.exs")))))
  :hook ((elixir-mode . (lambda ()
                          (add-hook 'before-save-hook #'elixir-format nil t)))
         (elixir-format . setup-elixir-format-arguments)
         (elixir-mode . lsp)
         (elixir-mode . exunit-mode))
  :init
  (advice-add 'elixir-format :around #'set-default-directory-to-mix-project-root))

(req-package flycheck-credo
  :ensure t
  :pin melpa
  :require flycheck validate
  :init
  (flycheck-credo-setup)
  :config
  (validate-setq flycheck-elixir-credo-strict t))

(req-package exunit
  :ensure t
  :bind (:map elixir-mode-map
              ("C-c , a" . exunit-verify-all)
              ("C-c , u" . exunit-verify-all-in-umbrella)
              ("C-c , v" . exunit-verify)
              ("C-c , s" . exunit-verify-single)
              ("C-c , r" . exunit-rerun)
              ("C-c , t" . exunit-toggle-file-and-test)
              ("C-c , o" . exunit-toggle-file-and-test-other-window)))

(provide 'init-elixir)
;;; init-elixir.el ends here
