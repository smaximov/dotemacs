;;; init-racket.el --- a major mode to edit Racket source files -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package racket-mode
  :pin melpa
  :ensure t
  :require paredit
  :hook ((racket-mode  . racket-unicode-input-method-enable)
         (racket-repl-mode . racket-unicode-input-method-enable)
         (racket-mode . enable-paredit-mode)))

(provide 'init-racket)
;;; init-racket.el ends here
