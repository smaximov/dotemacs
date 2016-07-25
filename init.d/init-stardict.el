;;; init-stardict.el --- configure `stardict' -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package stardict :loader :path
  :load-path "lib/stardict"
  :config
  (setf stardict:dictionary-path "~/.local/share/stardict/dic"))

(provide 'init-stardict)
;;; init-stardict.el ends here
