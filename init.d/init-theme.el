;;; init-theme.el --- configure Emacs theme -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(defvar nameless:theme-gui-loaded nil)
(defvar nameless:theme-terminal-loaded nil)

(defun nameless:load-theme-once (theme)
  "Ensure THEME is loaded only once."
  (if (window-system)
      (unless nameless:theme-gui-loaded
        (if nameless:theme-terminal-loaded
            (enable-theme theme)
          (load-theme theme t))
        (setf nameless:theme-gui-loaded t))
    (unless nameless:theme-terminal-loaded
      (if nameless:theme-gui-loaded
          (enable-theme theme)
        (load-theme theme t))
      (setf nameless:theme-terminal-loaded t))))

(req-package material-theme
  :config
  (with-daemon
   (nameless:load-theme-once 'material)))

(provide 'init-theme)
;;; init-theme.el ends here
