;;; init-ivy.el --- Ivy, Swiper, Counsel and stuff -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package ivy
  :ensure t
  :pin melpa-stable
  :require diminish validate tramp
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :bind (("C-c r" . ivy-resume)
	 :map ivy-minibuffer-map
	 ("C-c o" . ivy-occur))
  :config
  (validate-setq ivy-use-virtual-buffers t
                 ivy-height 10))

(req-package swiper
  :ensure t
  :pin melpa-stable
  :bind (([remap isearch-forward] . swiper)
         ("C-S-s" . swiper-all)))

(req-package counsel
  :ensure t
  :pin melpa-stable
  :bind (([remap execute-extended-command] . counsel-M-x)        ; M-x
         ([remap describe-function] . counsel-describe-function) ; C-h f
         ([remap describe-variable] . counsel-describe-variable) ; C-h v
         ([remap describe-bindings] . counsel-descbinds)         ; C-h b
         ([remap find-file] . counsel-find-file)                 ; C-x C-f
         ([remap yank-pop] . counsel-yank-pop)                   ; M-y
         ([remap switch-to-buffer] . counsel-switch-buffer)      ; C-x b
         ([remap switch-to-buffer-other-window] . counsel-switch-buffer-other-window) ; C-x 4 b
         ("C-c I" . counsel-info-lookup-symbol)
         ("C-c i" . counsel-imenu)
         ("C-c g" . counsel-rg)
         ("C-c u" . counsel-unicode-char)
         ("C-c P" . counsel-list-processes)
         ("C-c t" . counsel-recentf)

         ("C-h F" . find-function)
         ("C-h V" . find-variable)

         :map read-expression-map
         ("C-r" . counsel-expression-history)))

(req-package counsel-projectile
  :ensure t
  :pin melpa-stable
  :hook (after-init . counsel-projectile-mode))

(provide 'init-ivy)
;;; init-ivy.el ends here
