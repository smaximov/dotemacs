;;; init-helpers.el --- load and configure helpers utilities -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(req-package navigation
  :load-path "lib"
  :bind (("C-c f u i" . nameless/find-user-init-file)
         ("C-c f s" . nameless/find-scratch-buffer)
         ("C-c f d f" . nameless/find-dominating-file)
         ("C-x C-t" . nameless/find-term-buffer)))

(req-package file-helpers
  :load-path "lib"
  :hook ((after-save . nameless/file-make-executable-if-shebang)
         (after-save . nameless/set-auto-mode)))

(req-package phi-search
  :ensure t
  :pin melpa
  :init
  (require 'phi-replace)
  :bind
  (([remap query-replace] . phi-replace)))

(req-package diminish
  :ensure t
  :pin melpa-stable)

(req-package whitespace
  :ensure t
  :require diminish
  :diminish whitespace-mode
  :hook ((prog-mode markdown-mode) . whitespace-mode)
  :config
  (setf whitespace-line-column 120))

(req-package pixel-scroll
  :hook (prog-mode . pixel-scroll-mode))

(req-package display-line-numbers
  :preface
  (defconst display-line-numbers::relative-type-threshold 1000)
  :hook (prog-mode . (lambda ()
                       (setf display-line-numbers
                             (if (>= (count-lines (point-min) (point-max))
                                    display-line-numbers::relative-type-threshold)
                                 'relative
                               t)))))

(req-package validate
  :ensure t)

(req-package eldoc
  :require diminish
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode ielm-mode) . eldoc-mode))

(req-package term
  :hook (term-mode . (lambda () (setf yas-dont-activate-functions t)))
  :bind
  (("C-x t" . ansi-term)))

(req-package multiple-cursors
  :ensure t
  :pin melpa-stable
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(provide 'init-helpers)
;;; init-helpers.el ends here
