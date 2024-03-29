;;; init-magit.el --- `magit' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package magit
  :pin melpa-stable
  :ensure t
  :require validate
  :preface
  (defconst nameless:magit-dotjira ".jira")

  (defun nameless:magit-insert-current-jira-issue-number ()
    "Get current current JIRA issue number from branch name and insert it at point."
    (interactive)

    (if (or current-prefix-arg
            (plusp (minibuffer-depth)))
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
    (when-let* ((root (locate-dominating-file default-directory
                                              nameless:magit-dotjira))
                (dotjira (concat root nameless:magit-dotjira)))
      (if (file-exists-p dotjira)
          (insert (format "%s-"
                          (string-trim-right
                           (with-temp-buffer
                             (insert-file-contents dotjira)
                             (buffer-string)))))
        (user-error "Unable to infer JIRA issue prefix"))))

  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch-popup)
         ("C-x j" . nameless:magit-insert-current-jira-issue-number))
  :hook ((after-init . global-magit-file-mode)
         (git-commit-mode . (lambda ()
                              (validate-setq fill-column 72))))
  :config
  (validate-setq git-commit-summary-max-length 72
                 magit-completing-read-function #'ivy-completing-read))

(req-package forge
  :pin melpa-stable
  :ensure t
  :require magit
  :preface
  (defun nameless:toggle-gitlab-merge-request-draft-status ()
    "Toggle GitLab's merge request draft status in the pull request creation buffer."
    (interactive)
    (save-excursion
      (goto-char (+ (point-min) 2))
      (save-match-data
        (if (looking-at "Draft: ")
            (replace-match "")
          (insert "Draft: ")))))
  :bind (:map forge-post-mode-map
              ("C-x d" . nameless:toggle-gitlab-merge-request-draft-status)))

(provide 'init-magit)
;;; init-magit.el ends here
