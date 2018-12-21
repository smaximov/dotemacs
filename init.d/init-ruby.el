;;; init-ruby.el --- Ruby, Rails, and supporting tools -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package inf-ruby
  :ensure t
  :pin melpa-stable
  :require ruby-mode validate
  :hook (ruby-mode . inf-ruby-minor-mode)
  :config
  ;; may as well switch default implementaion to "ruby" (== "irb")
  (validate-setq inf-ruby-default-implementation "pry"
                 ruby-insert-encoding-magic-comment nil))

(req-package mmm-erb
  :mode  ("\\.ejs$" . html-erb-mode)
  :mode ("\\.erb$" . html-erb-mode))

(req-package ruby-mode ; built-in
  :ensure t
  :require smartparens autoinsert
  :mode "^Gemfile$"
  :mode "^Rakefile$"
  :hook (ruby-mode . smartparens-mode)
  :config
  (define-auto-insert '(ruby-mode . "Ruby skeleton")
    '(nil
      "# frozen_string_literal: true" ?\n
      _)))

(req-package rvm
  :ensure t
  :disabled
  :pin melpa-stable
  :require exec-path-from-shell
  :hook (after-init . rvm-use-default))

(req-package robe
  :ensure t
  :pin melpa-stable
  :require company ruby-mode rvm inf-ruby diminish
  :diminish robe-mode
  :hook (ruby-mode . robe-mode)
  :config
  (push 'company-robe company-backends)

  (advice-add 'inf-ruby-console-auto
              :before #'rvm-activate-corresponding-ruby))

(req-package yard-mode
  :ensure t
  :pin melpa
  :require ruby-mode diminish
  :diminish yard-mode
  :hook ruby-mode)

(req-package rake
  :ensure t
  :pin melpa-stable
  :require ruby-mode ivy
  :config
  (setf rake-completion-system 'ivy-completing-read)
  :bind (("C-c . t"  . rake)
         ("C-c . r" . rake-rerun)
         ("C-c . c" . rake-regenerate-cache)))

(req-package projectile-rails
  :ensure t
  :pin melpa-stable
  :disabled
  :require rvm validate
  :bind (:map projectile-rails-command-map
              ("#" . rvm-activate-corresponding-ruby))
  :preface
  ;; https://github.com/bbatsov/projectile/issues/991#issuecomment-248026667
  (defvar projectile-rails-keymap-prefix (kbd "C-c C-r"))
  :hook ((projectile-mode . projectile-rails-global-mode)
         (projectile-rails-server-mode . (lambda ()
                                           (setq-local compilation-scroll-output t))))
  :config
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
  :hook (projectile-mode . projectile-rails-or-hanami-on)
  :bind (:map projectile-hanami-mode-command-map
              ("#" . rvm-activate-corresponding-ruby)))

(req-package rspec-mode
  :ensure t
  :pin melpa-stable)

(req-package slim-mode
  :ensure t
  :pin melpa-stable)

(req-package rubocop-toggle-cops
  :load-path "lib"
  :require flycheck ruby-mode
  :bind (:map ruby-mode-map
              ("C-c :" . rubocop-toggle-cops)))

(req-package rubocop
  :ensure t
  :require ruby-mode diminish
  :diminish rubocop-mode
  :pin melpa-stable
  :hook (ruby-mode . rubocop-mode))

(provide 'init-ruby)
;;; init-ruby.el ends here
