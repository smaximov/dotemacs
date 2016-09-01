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

(defun nameless/find-scratch-buffer ()
  "Switch to the scratch buffer (create one if necessary).

With prefix argument, switch to the scratch buffer in other window."
  (interactive)
  (nameless/dispatch-by-prefix-arg #'switch-to-buffer-other-window #'switch-to-buffer
                                   "*scratch*"))

(defun nameless/find-user-init-file ()
  "Find user's initialization file.

With prefix argument, open the file in other window."
  (interactive)
  (nameless/dispatch-by-prefix-arg #'find-file-other-window #'find-file
                                   user-init-file))

(defun nameless/find-term-buffer ()
  "Switch to the latest TERM buffer, if any, or create one."
  (interactive)
  (--if-let (get-buffer "*ansi-term*")
      (if (comint-check-proc it)
          ;; the process is running
          (pop-to-buffer it)
        ;; the process is dead, reuse its buffer
        (kill-buffer it)
        (ansi-term (getenv "SHELL")))
    (ansi-term (getenv "SHELL"))))

(defun nameless/find-dominating-file (file)
  "Find FILE in directories up to root.

With prefix argument, open the file in other window."
  (interactive "sFile name: ")
  (--if-let (locate-dominating-file (buffer-file-name) file)
      (nameless/dispatch-by-prefix-arg #'find-file-other-window #'find-file
                                       (f-join it file))
    (error "Cannot find `%s'" file)))

(provide 'navigation)
;;; navigation.el ends here
