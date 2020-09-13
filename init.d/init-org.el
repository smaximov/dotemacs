;;; init-org.el --- configure Org mode and related packages  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package htmlize
  :ensure t
  :pin melpa-stable)

(req-package org
  :require f validate
  :init
  (require 'org-agenda)
  :config
  :force t
  (validate-setq org-directory "~/share/sync/notes"
                 org-default-notes-file (f-join org-directory "general.org")
                 org-agenda-files `(,org-default-notes-file)
                 org-src-fontify-natively t
                 org-enforce-todo-checkbox-dependencies t
                 org-enforce-todo-dependencies t
                 org-agenda-dim-blocked-tasks t
                 org-archive-location (f-join org-directory "archive.org::* From %s")
                 org-log-done 'time

                 ;; Don't align a node's content with the headline
                 org-adapt-indentation nil

                 org-goto-auto-isearch nil)
  ;; Custom agenda views
  (setf org-agenda-custom-commands
        '(("h" "Agenda and home-related chores"
           ((agenda)
            (tags-todo "home"
                       ((org-agenda-sorting-strategy '(priority-down))))))))

  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c f a" . org-cycle-agenda-files)
         ("C-c l" . org-store-link)))

(req-package org-page
  :disabled
  :load-path "lib/org-page"
  :if (file-exists-p (expand-file-name "lib/org-page/org-page.el" user-emacs-directory))
  :require dash ht validate htmlize
  :config
  (validate-setq op/site-preview-directory "/tmp/org-page-preview"
                 op/repository-directory "~/src/maximov.space"

                 op/site-domain "https://maximov.space"
                 op/site-main-title "Untitled"
                 op/site-sub-title "Emacs, Programming, and Anything"

                 op/highlight-render 'htmlize

                 op/theme 'mdo
                 op/theme-root-directory "~/src/maximov.space/themes"

                 op/personal-disqus-shortname "smaximov"
                 op/personal-github-link "https://github.com/smaximov"
                 op/personal-google-analytics-id "UA-74709646-1"))

(req-package zpresent
  :pin melpa
  :require org
  :ensure t)

(provide 'init-org)
;;; init-org.el ends here
