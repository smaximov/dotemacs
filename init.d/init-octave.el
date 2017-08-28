;;; init-octave.el --- octave configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package octave
  :disabled
  :mode ("\\.m$" . octave-mode))

(provide 'init-octave)
;;; init-octave.el ends here
