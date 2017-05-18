;;; init-path-from-shell.el --- configure `exec-path-from-shell'  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "CARGO_HOME")
  (add-to-list 'exec-path-from-shell-variables "RUSTUP_HOME")
  (add-to-list 'exec-path-from-shell-variables "XDG_CACHE_HOME")
  (with-daemon
   (exec-path-from-shell-initialize)))

(provide 'init-path-from-shell)
;;; init-path-from-shell.el ends here
