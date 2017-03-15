;;; init-ruby.el --- Ruby, Rails, and supporting tools -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package inf-ruby
  :require ruby-mode validate
  :init
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  :config
  ;; may as well switch default implementaion to "ruby" (== "irb")
  (validate-setq inf-ruby-default-implementation "pry"
                 ruby-insert-encoding-magic-comment nil))

(req-package ruby-mode :loader :built-in
  :require smartparens autoinsert
  :mode "^Gemfile$"
  :mode "^Rakefile$"
  :init
  (add-hook 'ruby-mode-hook #'smartparens-mode)
  :config
  (define-auto-insert '(ruby-mode . "Ruby skeleton")
    '(nil
      "# frozen_string_literal: true" ?\n
      _)))

(req-package rvm
  :require exec-path-from-shell
  :init
  (add-hook 'after-init-hook #'rvm-use-default))

(req-package rbenv :force t
  :require exec-path-from-shell-arguments
  :init
  (add-hook 'after-init-hook #'global-rbenv-mode)
  (add-hook 'after-init-hook #'rbenv-use-global))

(req-package robe
  :require company ruby-mode rvm inf-ruby diminish
  :diminish robe-mode
  :init
  (add-hook 'ruby-mode-hook #'robe-mode)
  :config
  (push 'company-robe company-backends)

  (advice-add 'inf-ruby-console-auto
              :before #'rvm-activate-corresponding-ruby))

(req-package yard-mode
  :require ruby-mode diminish
  :diminish yard-mode
  :init
  (add-hook 'ruby-mode-hook #'yard-mode))

(req-package rake
  :require ruby-mode ivy
  :config
  (setf rake-completion-system 'ivy-completing-read)
  :bind (("C-c . t"  . rake)
         ("C-c . r" . rake-rerun)
         ("C-c . c" . rake-regenerate-cache)))

(req-package projectile-rails
  :require rvm validate
  :bind (:map projectile-rails-command-map
              ("#" . rvm-activate-corresponding-ruby))
  :preface
  ;; https://github.com/bbatsov/projectile/issues/991#issuecomment-248026667
  (defvar projectile-rails-keymap-prefix (kbd "C-c C-r"))
  :init
  (add-hook 'projectile-mode-hook #'projectile-rails-global-mode)
  :config
  (add-hook 'projectile-rails-server-mode-hook
            (lambda ()
              (setq-local compilation-scroll-output t)))
  (validate-setq projectile-rails-expand-snippet nil))

(req-package projectile-hanami
  :require rvm validate
  :disabled
  :preface
  (defun projectile-rails-or-hanami-on ()
    "Activate either `projectile-rails-mode` or `projectile-hanami-mode`."
    (if (projectile-hanami-applicable-p)
        (projectile-hanami-mode +1)
      (projectile-rails-on)))
  :init
  (add-hook 'projectile-mode-hook #'projectile-rails-or-hanami-on)
  :bind (:map projectile-hanami-mode-command-map
              ("#" . rvm-activate-corresponding-ruby)))

(req-package rspec-mode)

(req-package slim-mode)

(req-package rubocop-toggle-cops
  :loader :path
  :load-path "lib"
  :require flycheck ruby-mode
  :bind (:map ruby-mode-map
              ("C-c :" . rubocop-toggle-cops)))

(provide 'init-ruby)
;;; init-ruby.el ends here
