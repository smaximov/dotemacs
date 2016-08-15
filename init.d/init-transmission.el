;;; init-transmission.el --- an Emacs interface to Transmission daemon  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package transmission
  :loader :path
  :load-path "lib/transmission"
  :commands (transmission transmission-add)
  :preface
  ;; Work around https://github.com/holomorph/transmission/issues/2
  (defun transmission-add-magnet (magnet)
    "Add a torrent by MAGNET link."
    (interactive "sMagnet URI: ")
    (transmission-add magnet))
  :config
  (setf transmission-host "my.keenetic.net"
        transmission-service 8090
        transmission-rpc-path "/transmission/rpc"
        ;; password is read from ~/.netrc
        transmission-rpc-auth '(:username "admin")
        transmission-refresh-modes '(transmission-mode
                                     transmission-files-mode
                                     transmission-info-mode
                                     transmission-peers-mode))
  :bind (:map transmission-mode-map
              ("A" . transmission-add-magnet)))

(provide 'init-transmission)
;;; init-transmission ends here
