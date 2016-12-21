;;; rubocop-toggle-cops.el --- toggle cops at point -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'flycheck)

(defconst rubocop-inline-directive
  "[[:blank:]]*#[[:blank:]]*rubocop:disable.*$"
  "A regular expression to check for the rubocop inline directive comment.")

(defun rubocop-inline-directive-p ()
  "Return if the current line contains the rubocop inline directive comment."
  (save-excursion
    (re-search-forward rubocop-inline-directive (point-at-eol) t)))

(defun rubocop-toggle-cops ()
  "Toggle the rubocop inline directive comment at the current line."
  (interactive)
  (save-match-data
    (if (rubocop-inline-directive-p)
        (delete-region (match-beginning 0) (match-end 0))
      (let ((cops (rubocop-cops (flycheck-overlay-errors-in (point-at-bol)
                                                            (point-at-eol)))))
        (if (null cops)
            (message "No cops found at point")
          (save-excursion
            (end-of-line)
            (insert " # rubocop:disable "
                    (mapconcat #'identity cops ", "))))))))

(defun rubocop-cops (flycheck-errors)
  "Filter cop names from FLYCHECK-ERRORS."
  (let ((combine (lambda (acc err)
                   (when (eq (flycheck-error-checker err)
                             'ruby-rubocop)
                     (let ((id (flycheck-error-id err)))
                       (when id
                         (cons id acc)))))))
    (reverse (seq-uniq (seq-reduce combine flycheck-errors (list))))))

(provide 'rubocop-toggle-cops)
;;; rubocop-toggle-cops.el ends here
