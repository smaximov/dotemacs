;;; init-tramp.el --- transparently access remote files from within Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package tramp
  :pin gnu
  :ensure t
  :init
  (connection-local-set-profile-variables
   'remote-without-auth-sources '((auth-sources . nil)))

  (connection-local-set-profiles
   '(:application tramp) 'remote-without-auth-sources)

  ;; HACK: Ivy still uses this variable, but the new version of Tramp (2.5)
  ;;   doesn't expose it, which leads an Ivy error when invoking Tramp sudo
  ;;   password prompt. Probably delete this hack after a couple new Ivy
  ;;   versions.
  (if (boundp 'tramp-message-show-message)
      (setf tramp-message-show-message nil)
    (defvar tramp-message-show-message nil)))

(provide 'init-tramp)
;;; init-tramp.el ends here
