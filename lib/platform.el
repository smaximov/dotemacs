;;; platform.el --- platform-specific code -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun nameless:platform:desktop ()
  "Return the name of the running DE or WM."
  (getenv "GDMSESSION"))

(defvar nameless:platform:tiling-desktops
  (list "awesome"))

(defun nameless:platform:tiling-p ()
  "Return true if running inside one of tiling WMs."
  (member (nameless:platform:desktop)
          nameless:platform:tiling-desktops))

(provide 'platform)
;;; platform.el ends here
