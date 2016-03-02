;;; navigation.el --- Custom commands for finding different files

;;; Commentary:

;;; Code:

(require 'f)
(require 'dash)

(defun nameless/dispatch-by-prefix-arg (prefix-present-fun prefix-absent-fun &rest args)
  "Choose function based on the presence of prefix argument.

If the command invoked with prefix argument, PREFIX-PRESENT-FUN is called,
PREFIX-ABSENT-FUN otherwise, with  ARGS as their arguments."
  (apply (if current-prefix-arg prefix-present-fun prefix-absent-fun) args))

;;;###autoload
(defun nameless/find-scratch-buffer ()
  "Switch to the scratch buffer (create one if necessary).

With prefix argument, switch to the scratch buffer in other window."
  (interactive)
  (nameless/dispatch-by-prefix-arg #'switch-to-buffer-other-window #'switch-to-buffer
                                   "*scratch*"))

;;;###autoload
(defun nameless/find-user-init-file ()
  "Find user's initialization file.

With prefix argument, open the file in other window."
  (interactive)
  (nameless/dispatch-by-prefix-arg #'find-file-other-window #'find-file
                                   user-init-file))

(provide 'navigation)
;;; navigation.el ends here
