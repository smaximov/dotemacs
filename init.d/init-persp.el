;;; init-persp.el --- manage workspaces AKA perspectives -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package persp-mode
  :disabled
  :require ivy
  :preface
  (defvar persp-keymap-prefix (kbd "C-x x"))
  :init
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1)))
  (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))
  :bind (([remap kill-buffer] . persp-kill-buffer))
  :config
  (setf persp-autokill-buffer-on-remove 'kill-weak
        ivy-sort-functions-alist
        (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil)))))

(provide 'init-persp)
;;; init-persp.el ends here
