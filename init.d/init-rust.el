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
  :require rust-mode eldoc exec-path-from-shell validate
  :diminish racer-mode
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode))
  :config
  (validate-setq racer-cmd (concat racer-cargo-home "/bin/racer")
                 racer-rust-src-path (concat (string-trim-right (shell-command-to-string "rustc --print sysroot"))
                                             "/lib/rustlib/src/rust/src")))

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
                              (cargo.toml? (and file
                                                (string-equal "Cargo.toml" file))))
                         (when cargo.toml?
                           (cargo-minor-mode))))))

(provide 'init-rust)
;;; init-rust.el ends here
