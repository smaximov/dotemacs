;;; init-org.el --- configure Org mode and related packages  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package htmlize
  :ensure t
  :pin melpa-stable)

(req-package org
  :require validate
  :init
  (require 'org-agenda)
  :config
  (validate-setq org-directory "~/share/sync/notes/"
                 org-default-notes-file (concat org-directory "general.org")
                 org-agenda-files `(,org-default-notes-file)
                 org-src-fontify-natively t
                 org-enforce-todo-checkbox-dependencies t
                 org-enforce-todo-dependencies t
                 org-agenda-dim-blocked-tasks t
                 org-archive-location (concat org-directory "archive.org::* From %s")
                 org-log-done 'time

                 ;; Don't align a node's content with the headline
                 org-adapt-indentation nil

                 org-goto-auto-isearch nil)
  ;; Custom agenda views
  (setf org-agenda-custom-commands
        '(("h" "Agenda and home-related chores"
           ((agenda)
            (tags-todo "chore"
                       ((org-agenda-sorting-strategy '(priority-down))))))))

  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c f a" . org-cycle-agenda-files)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c !" . org-time-stamp-inactive)))

(req-package zpresent
  :pin melpa
  :require org
  :ensure t)

(provide 'init-org)
;;; init-org.el ends here
