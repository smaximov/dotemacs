;;; init-flycheck.el --- configure `flycheck'        -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package flycheck
  :ensure t
  :require diminish direnv
  :diminish flycheck-mode
  :pin melpa-stable
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-command-wrapper-function
   (lambda (command)
     (let ((bundler (executable-find "bundle"))
           (cmd (file-name-nondirectory (car command))))
       (if (string-equal cmd "rubocop")
           (append (list bundler "exec" "rubocop") (cdr command))
         command)))))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
