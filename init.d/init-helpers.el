;;; init-helpers.el --- load and configure helpers utilities -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(req-package navigation :loader :path
  :load-path "lib"
  :bind (("C-c f u i" . nameless/find-user-init-file)
         ("C-c f s" . nameless/find-scratch-buffer)
         ("C-c f d f" . nameless/find-dominating-file)
         ("C-x C-t" . nameless/find-term-buffer)))

(req-package file-helpers :loader :path
  :load-path "lib"
  :init
  (add-hook 'after-save-hook #'nameless/file-make-executable-if-shebang)
  (add-hook 'after-save-hook #'nameless/set-auto-mode))

(req-package phi-search
  :init
  (require 'phi-replace)
  :bind
  (([remap query-replace] . phi-replace)))

(req-package f)

(req-package s)

(req-package diminish)

(req-package async)

(req-package dash)

(req-package whitespace
  :require diminish
  :diminish whitespace-mode
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  (add-hook 'markdown-mode-hook #'whitespace-mode)
  :config
  (setf whitespace-line-column 120))

(req-package tldr
  :bind ("C-c t l d r" . tldr))

(req-package eldoc
  :require diminish dash
  :diminish eldoc-mode
  :init
  (--each '(emacs-lisp-mode-hook ielm-mode-hook)
    (add-hook it #'eldoc-mode)))

(req-package term :loader :built-in
  :init
  (add-hook 'term-mode-hook (lambda ()
                              (setf yas-dont-activate-functions t)))
  :bind
  (("C-x t" . ansi-term)))



(req-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(provide 'init-helpers)
;;; init-helpers.el ends here
