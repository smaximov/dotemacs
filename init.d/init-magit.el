;;; init-magit.el --- `magit' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package magit
  :pin melpa-stable
  :ensure t
  :preface
  (defconst nameless:magit-dotjira ".jira")

  (defun nameless:magit-insert-current-jira-issue-number ()
    "Get current current JIRA issue number from branch name and insert it at point."
    (interactive)
    (if current-prefix-arg
        (nameless:magit-insert-current-jira-issue-prefix)
      (let ((branch (magit-get-current-branch)))
        (if branch
            (save-match-data
              (if (string-match "^\\([A-Z]+\\)[-_]\\([[:digit:]]+\\)" branch)
                  (insert (format "%s-%s "
                                  (match-string-no-properties 1 branch)
                                  (match-string-no-properties 2 branch)))
                (nameless:magit-insert-current-jira-issue-prefix)))
          (user-error "This branch doesn't reference a JIRA issue")))))

  (defun nameless:magit-insert-current-jira-issue-prefix ()
    (interactive)
    (-when-let* ((root (locate-dominating-file default-directory
                                               nameless:magit-dotjira))
                 (dotjira (concat root nameless:magit-dotjira)))
      (if (file-exists-p dotjira)
          (insert (format "%s-"
                          (string-trim-right
                           (with-temp-buffer
                             (insert-file-contents dotjira)
                             (buffer-string)))))
        (user-error "Unable to infer JIRA issue number"))))

  :require validate dash f
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch-popup)
         ("C-x j" . nameless:magit-insert-current-jira-issue-number))
  :if (version<= "24.4" emacs-version)
  :hook (after-init . global-magit-file-mode)
  :config
  (validate-setq git-commit-summary-max-length 100
                 magit-completing-read-function #'ivy-completing-read))

(req-package magithub
  :pin melpa-stable
  :ensure t
  :require magit exec-path-from-shell validate
  :config
  (magithub-feature-autoinject t)
  (validate-setq magithub-dir
                 (expand-file-name "magithub" (let ((cache-home (or (getenv "XDG_CACHE_HOME")
                                                                    (expand-file-name "~/.cache"))))
                                                (make-directory cache-home t)
                                                cache-home))))

(provide 'init-magit)
;;; init-magit.el ends here
