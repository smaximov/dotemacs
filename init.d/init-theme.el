;;; init-theme.el --- configure Emacs theme -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package material-theme
  :config
  (with-daemon
   (load-theme 'material t)))

(provide 'init-theme)
;;; init-theme.el ends here
