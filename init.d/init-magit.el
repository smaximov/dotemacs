;;; init-magit.el --- `magit' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package magit
  :preface
  (defun nameless:magit-insert-current-jira-issue-number ()
    "Get current current JIRA issue number from branch name and insert it at point."
    (interactive)
    (let ((branch (magit-get-current-branch)))
      (if branch
          (save-match-data
            (if (string-match "^\\([A-Z]+\\)[-_]\\([[:digit:]]+\\)" branch)
                (insert (format "%s-%s "
                                (match-string-no-properties 1 branch)
                                (match-string-no-properties 2 branch)))
              (user-error "This branch doesn't reference a JIRA issue")))
        (user-error "There is no current branch"))))
  :require validate
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch-popup)
         ("C-x j" . nameless:magit-insert-current-jira-issue-number))
  :if (version<= "24.4" emacs-version)
  :init
  (add-hook 'after-init-hook #'global-magit-file-mode)
  :config
  (validate-setq git-commit-summary-max-length 100
                 magit-completing-read-function #'ivy-completing-read))

(req-package magithub
  :require magit exec-path-from-shell validate
  :config
  (magithub-feature-autoinject t)
  (validate-setq magithub-dir
                 (expand-file-name "magithub" (let ((cache-home (or (getenv "XDG_CACHE_HOME")
                                                                    (expand-file-name "~/.cache"))))
                                                (make-directory cache-home t)
                                                cache-home)))
  :pin melpa-stable)

(provide 'init-magit)
;;; init-magit.el ends here
