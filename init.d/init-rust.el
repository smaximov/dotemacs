;;; init-rust.el --- `rust-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package rust-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.rs$"
  :hook (rust-mode . electric-pair-mode))

(req-package cargo
  :ensure t
  :pin melpa-stable
  :require rust-mode diminish
  :diminish cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(req-package racer
  :ensure t
  :pin melpa-stable
  :require rust-mode exec-path-from-shell
  :diminish racer-mode
  :hook (rust-mode . racer-mode))

(req-package flycheck-rust
  :ensure t
  :pin melpa
  :require flycheck
  :hook (flycheck-mode . flycheck-rust-setup))

(req-package toml-mode
  :ensure t
  :pin melpa
  :require f cargo
  :mode "\\.cargo/config$"
  :hook (toml-mode . (lambda ()
                       (let* ((path (buffer-file-name))
                              (file (and path
                                         (f-filename path)))
                              (is-cargo.toml (and file
                                                  (string-equal "Cargo.toml" file))))
                         (when is-cargo.toml
                           (cargo-minor-mode))))))

(provide 'init-rust)
;;; init-rust.el ends here
