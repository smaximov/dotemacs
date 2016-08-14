;;; init-transmission.el --- an Emacs interface to Transmission daemon  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package transmission
  :commands (transmission transmission-add)
  :config
  (setf transmission-host "my.keenetic.net"
        transmission-service 8090
        transmission-rpc-path "/transmission/rpc"
        ;; password is read from ~/.netrc
        transmission-rpc-auth '(:username "admin")
        transmission-refresh-modes '(transmission-mode
                                     transmission-files-mode
                                     transmission-info-mode
                                     transmission-peers-mode)))

(provide 'init-transmission)
;;; init-transmission ends here
