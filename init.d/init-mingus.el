;;; init-mingus.el --- mingus is an Emacs interface to MPD -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package mingus-stays-home
  :ensure mingus
  :bind (("C-c m >" . mingus-next)
         ("C-c m <" . mingus-prev)
         ("C-c m p" . mingus)
         ("C-c m b" . mingus-browse)
         ("C-c m t" . mingus-toggle)))

(provide 'init-mingus)
;;; init-mingus.el ends here
