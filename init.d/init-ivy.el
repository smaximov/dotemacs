;;; init-ivy.el --- Ivy, Swiper, Counsel and stuff -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package counsel
  :require diminish
  :diminish ivy-mode
  :init
  (require 'ivy)
  (require 'swiper)
  (add-hook 'after-init-hook #'ivy-mode)
  :bind (([remap execute-extended-command] . counsel-M-x)        ; M-x
         ([remap describe-function] . counsel-describe-function) ; C-h f
         ([remap describe-variable] . counsel-describe-variable) ; C-h v
         ([remap describe-bindings] . counsel-descbinds)         ; C-h b
         ([remap find-file] . counsel-find-file)                 ; C-x C-f
         ([remap yank-pop] . counsel-yank-pop)                   ; M-y
         ("C-c I" . counsel-info-lookup-symbol)
         ("C-c i" . counsel-imenu)
         ("C-c g" . counsel-ag)
         ("C-c u" . counsel-unicode-char)
         ("C-c P" . counsel-list-processes)
         ("C-c t" . counsel-recentf)

         ([remap switch-to-buffer] . ivy-switch-buffer)          ; C-x b
         ([remap list-buffers] . ivy-switch-buffer)              ; C-x C-b
         ("C-c r" . ivy-resume)

         ([remap isearch-forward] . swiper)

         :map ivy-minibuffer-map
         ("C-c o" . ivy-occur)

         :map read-expression-map
         ("C-r" . counsel-expression-history))
  :config
  (setf ivy-use-virtual-buffers t
        ivy-height 30))

(req-package counsel-projectile
  :require counsel
  :init
  (add-hook 'after-init-hook #'counsel-projectile-on))

(provide 'init-ivy)
;;; init-ivy.el ends here