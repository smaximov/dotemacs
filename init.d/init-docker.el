;;; init-docker.el --- `docker' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package dockerfile-mode
  :pin melpa
  :ensure t)

(provide 'init-docker)
;;; init-docker.el ends here
