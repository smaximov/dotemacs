;;; init-rust.el --- `rust-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package rust-mode
  :init
  (add-hook 'rust-mode-hook #'electric-pair-mode))

(req-package cargo
  :require rust-mode diminish
  :diminish cargo-minor-mode
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(req-package racer
  :require rust-mode f eldoc exec-path-from-shell
  :diminish racer-mode
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  :config
  (setf racer-rust-src-path (f-full "~/src/rust/src")
        racer-cmd (executable-find "racer")))

(req-package flycheck-rust
  :require flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(req-package toml-mode
  :require f cargo
  :mode "\\.cargo/config$"
  :init
  (add-hook 'toml-mode-hook (lambda ()
                              (let* ((path (buffer-file-name))
                                     (file (and path
                                                (f-filename path)))
                                     (is-cargo.toml (and file
                                                    (s-equals? "Cargo.toml" file))))
                                (when is-cargo.toml
                                        (cargo-minor-mode))))))

(provide 'init-rust)
;;; init-rust.el ends here
