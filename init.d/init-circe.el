;;; init-circe.el --- Circe is a Client for IRC in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package circe
  :require validate
  :config
  (validate-setq circe-network-options
                 '(("Freenode"
                    :tls t
                    :nick "smaximov"))))

(provide 'init-circe)
;;; init-circe.el ends here
