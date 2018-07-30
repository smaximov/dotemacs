;;; init-racket.el --- a major mode to edit Racket source files -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package racket-mode
  :ensure t
  :hook ((racket-mode . racket-unicode-input-method-enable)
         (racket-repl-mode . racket-unicode-input-method-enable)))

(provide 'init-racket)
;;; init-racket.el ends here
