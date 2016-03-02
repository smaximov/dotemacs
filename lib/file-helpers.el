;;; file-helpers.el --- file-related helper functions

;;; Commentary:

;;; Code:

(require 'f)

(defun nameless/file-make-executable (file)
  "Make FILE executable."
  (interactive "fSelect file: ")
  (let* ((file-modes (file-modes file))
         (new-modes (logior #o100 file-modes)))
    (set-file-modes file new-modes)
    (message "Set mode 0%s (octal) for file `%s'" new-modes file)))

(defun nameless/file-make-executable-if-shebang ()
  "Make the file associated with the current buffer executable if it has shebang."
  (and buffer-file-name
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (when (and (looking-at auto-mode-interpreter-regexp)
                      (not (f-executable? buffer-file-name)))
             (nameless/file-make-executable buffer-file-name))))))

(defun nameless/set-auto-mode ()
  "Call `set-auto-mode' if `major-mode' is `fundamental-mode'."
  (interactive)
  (when (equal major-mode 'fundamental-mode)
    (set-auto-mode t)))

(provide 'file-helpers)
;;; file-helpers.el ends here
